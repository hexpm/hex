defmodule Mix.Tasks.Hex.Package do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Fetches or diffs packages"

  @default_diff_command "git diff --no-index __PATH1__ __PATH2__"

  @doc false
  def default_diff_command(), do: @default_diff_command()

  @moduledoc """
  Fetches or diffs packages.

  ## Fetch package

  Fetch a package tarball to the current directory.

      mix hex.package fetch PACKAGE VERSION [--unpack] [--output PATH]

  You can pipe the fetched tarball to stdout by setting `--output -`.

  ## Fetch and diff package contents between versions

      mix hex.package diff PACKAGE VERSION1 VERSION2
      mix hex.package diff PACKAGE VERSION1..VERSION2

  This command fetches package tarballs for both versions,
  unpacks them into temporary directories and runs a diff command.
  Afterwards, the temporary directories are automatically deleted.

  Note, similarly to when tarballs are fetched with `mix deps.get`,
  a `hex_metadata.config` is placed in each unpacked directory.
  This file contains package's metadata as Erlang terms and so
  we can additionally see the diff of that.

  The exit code of the task is that of the underlying diff command.

  ### Diff command

  The diff command can be customized by setting `diff_command`
  configuration option, see `mix help hex.config` for more information.
  The default diff command is:

      #{@default_diff_command}

  The `__PATH1__` and `__PATH2__` placeholders will be interpolated with
  paths to directories of unpacked tarballs for each version.

  Many diff commands supports coloured output but because we execute
  the command in non-interactive mode, they'd usually be disabled.

  On Unix systems you can pipe the output to more commands, for example:

      mix hex.package diff decimal 1.0.0..1.1.0 | colordiff | less -R

  Here, the output of `mix hex.package diff` is piped to the `colordiff`
  utility to adds colours, which in turn is piped to `less -R` which
  "pages" it. (`-R` preserves escape codes which allows colours to work.)

  Another option is to configure the diff command itself. For example, to
  force Git to always colour the output we can set the `--color=always` option:

      mix hex.config diff_command "git diff --color=always --no-index __PATH1__ __PATH2__"
      mix hex.package diff decimal 1.0.0..1.1.0

  ## Command line options

    * `--unpack` - Unpacks the tarball after fetching it

    * `-o`, `--output` - Sets output path. When used with `--unpack` it means
      the directory (Default: `<app>-<version>`). Otherwise, it specifies
      tarball path (Default: `<app>-<version>.tar`)

    * `--organization ORGANIZATION` - Set this for private packages belonging to an organization

    * `--repo REPO` - Set this for self-hosted Hex instances, default: `hexpm`
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [unpack: :boolean, organization: :string, output: :string, repo: :string]
  @aliases [o: :output]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches, aliases: @aliases)
    unpack = Keyword.get(opts, :unpack, false)
    output = Keyword.get(opts, :output, nil)

    case args do
      ["fetch", package, version] ->
        fetch(repo(opts), package, version, unpack, output)

      ["diff", package, version1, version2] ->
        diff(repo(opts), package, "#{version1}..#{version2}")

      ["diff", package, version_range] ->
        diff(repo(opts), package, version_range)

      _ ->
        Mix.raise("""
          Invalid arguments, expected one of:

          mix hex.package fetch PACKAGE VERSION [--unpack]
          mix hex.package diff PACKAGE VERSION1 VERSION2
          mix hex.package diff PACKAGE VERSION1..VERSION2
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"fetch PACKAGE VERSION [--unpack]", "Fetch the package"},
      {"diff PACKAGE VERSION1 VERSION2", "Fetch and diff package contents between versions"},
      {"diff PACKAGE VERSION1..VERSION2", "Fetch and diff package contents between versions"}
    ]
  end

  defp fetch(repo, package, version, false, "-") do
    Hex.Registry.Server.open()
    Hex.Registry.Server.prefetch([{repo, package}])

    tarball = fetch_tarball!(repo, package, version)
    IO.binwrite(tarball)

    Hex.Registry.Server.close()
  end

  defp fetch(_repo, _package, _version, true, "-") do
    Mix.raise("Cannot unpack the package while output destination is stdout")
  end

  defp fetch(repo, package, version, unpack?, output) do
    Hex.Registry.Server.open()
    Hex.Registry.Server.prefetch([{repo, package}])

    tarball = fetch_tarball!(repo, package, version)
    if !is_nil(output), do: File.mkdir_p!(output)

    abs_name = Path.absname("#{package}-#{version}")

    {abs_path, tar_path} =
      if is_nil(output) do
        {abs_name, "#{abs_name}.tar"}
      else
        {output, Path.join(output, "#{package}-#{version}.tar")}
      end

    File.write!(tar_path, tarball)

    %{inner_checksum: inner_checksum, outer_checksum: outer_checksum} =
      Hex.unpack_tar!(tar_path, abs_path)

    verify_inner_checksum!(repo, package, version, inner_checksum)
    verify_outer_checksum!(repo, package, version, outer_checksum)

    message =
      if unpack? do
        File.rm!(tar_path)
        "#{package} v#{version} extracted to #{abs_path}"
      else
        "#{package} v#{version} downloaded to #{tar_path}"
      end

    Hex.Shell.info(message)
    Hex.Registry.Server.close()
  end

  defp fetch_tarball!(repo, package, version) do
    etag = nil

    case Hex.SCM.fetch(repo, package, version, :memory, etag) do
      {:ok, :new, tarball, _etag} ->
        tarball

      {:error, reason} ->
        Mix.raise(
          "Downloading " <>
            Hex.Repo.tarball_url(repo, package, version) <> " failed:\n\n" <> reason
        )
    end
  end

  defp verify_inner_checksum!(repo, package, version, checksum) do
    registry_checksum = Registry.inner_checksum(repo, package, version)

    if checksum != registry_checksum do
      Mix.raise("Checksum mismatch against registry (inner)")
    end
  end

  defp verify_outer_checksum!(repo, package, version, checksum) do
    registry_checksum = Registry.outer_checksum(repo, package, version)

    if checksum != registry_checksum do
      Mix.raise("Checksum mismatch against registry (outer)")
    end
  end

  defp diff(repo, package, version_range) do
    Hex.Registry.Server.open()
    Hex.Registry.Server.prefetch([{repo, package}])

    {version1, version2} = parse_version_range!(version_range)
    path1 = tmp_path("#{package}-#{version1}-")
    path2 = tmp_path("#{package}-#{version2}-")

    try do
      tarball1 = fetch_tarball!(repo, package, version1)
      tarball2 = fetch_tarball!(repo, package, version2)

      %{inner_checksum: inner_checksum, outer_checksum: outer_checksum} =
        Hex.unpack_tar!({:binary, tarball1}, path1)

      verify_inner_checksum!(repo, package, version1, inner_checksum)
      verify_outer_checksum!(repo, package, version1, outer_checksum)

      %{inner_checksum: inner_checksum, outer_checksum: outer_checksum} =
        Hex.unpack_tar!({:binary, tarball2}, path2)

      verify_inner_checksum!(repo, package, version2, inner_checksum)
      verify_outer_checksum!(repo, package, version2, outer_checksum)

      Hex.Registry.Server.close()

      cmd =
        Hex.State.fetch!(:diff_command)
        |> String.replace("__PATH1__", path1)
        |> String.replace("__PATH2__", path2)

      code = Mix.shell().cmd(cmd)
      Mix.Tasks.Hex.set_exit_code(code)
    after
      File.rm_rf!(path1)
      File.rm_rf!(path2)
    end
  end

  defp tmp_path(prefix) do
    random_string = Base.encode16(:crypto.strong_rand_bytes(4))
    Path.join(System.tmp_dir!(), prefix <> random_string)
  end

  defp parse_version_range!(string) do
    case String.split(string, "..", trim: true) do
      [version1, version2] ->
        version1 = Hex.Version.parse!(version1)
        version2 = Hex.Version.parse!(version2)
        {to_string(version1), to_string(version2)}

      _ ->
        Mix.raise(
          "Expected version range to be in format `VERSION1..VERSION2`, got: `#{inspect(string)}`"
        )
    end
  end

  defp repo(opts) do
    repo = Keyword.get(opts, :repo, "hexpm")

    if organization = opts[:organization] do
      Enum.join([repo, organization], ":")
    else
      repo
    end
  end
end
