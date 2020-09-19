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

      mix hex.package diff PACKAGE VERSION

  This command compares the package inside deps folder against
  with the target version, unpacking the target version into
  temparary directory and running a diff command.

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

  Many diff commands supports coloured output but becase we execute
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
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [unpack: :boolean, organization: :string, output: :string]
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

      ["diff", package, version] ->
        diff(repo(opts), package, parse_version!(version))

      _ ->
        Mix.raise("""
          Invalid arguments, expected one of:

          mix hex.package fetch PACKAGE VERSION [--unpack]
          mix hex.package diff PACKAGE VERSION1..VERSION2
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"fetch PACKAGE VERSION [--unpack]", "Fetch the package"},
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

  defp diff(repo, package, version) when is_binary(version) do
    check_valid_mix_project!(package, version)

    path_lock =
      case Map.get(Mix.Dep.Lock.read(), String.to_atom(package)) do
        nil ->
          Mix.raise(
            "Cannot find the package \"#{package}\" in \"mix.lock\" file. " <>
              "Please ensure it has been specified in \"mix.exs\" and run \"mix deps.get\""
          )

        _ ->
          unescaped_path = Path.join(Mix.Project.deps_path(), package)
          String.replace(unescaped_path, "\"", "\\\"")
      end

    path = tmp_path("#{package}-#{version}-")

    try do
      fetch_and_unpack!(repo, package, [{path, version}])
      code = run_diff_path!("\"#{path_lock}\"", path)
      Mix.Tasks.Hex.set_exit_code(code)
    after
      File.rm_rf!(path)
    end
  end

  defp diff(repo, package, {version1, version2}) do
    path1 = tmp_path("#{package}-#{version1}-")
    path2 = tmp_path("#{package}-#{version2}-")

    try do
      fetch_and_unpack!(repo, package, [{path1, version1}, {path2, version2}])

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

  defp fetch_and_unpack!(repo, package, versions) do
    Hex.Registry.Server.open()
    Hex.Registry.Server.prefetch([{repo, package}])

    try do
      Enum.each(versions, fn {path, version} ->
        tarball = fetch_tarball!(repo, package, version)
        %{inner_checksum: inner_checksum, outer_checksum: outer_checksum} = 
          Hex.unpack_tar!({:binary, tarball}, path)
        verify_inner_checksum!(repo, package, version, inner_checksum)
        verify_outer_checksum!(repo, package, version, outer_checksum)
      end)
    after
      Hex.Registry.Server.close()
    end
  end

  defp run_diff_path!(path1, path2) do
    cmd =
      Hex.State.fetch!(:diff_command)
      |> String.replace("__PATH1__", path1)
      |> String.replace("__PATH2__", path2)

    Mix.shell().cmd(cmd)
  end

  defp tmp_path(prefix) do
    random_string = Base.encode16(:crypto.strong_rand_bytes(4))
    Path.join(System.tmp_dir!(), prefix <> random_string)
  end

  defp parse_version!(string) do
    case String.split(string, "..", trim: true) do
      [version1, version2] ->
        version1 = Hex.Version.parse!(version1)
        version2 = Hex.Version.parse!(version2)
        {to_string(version1), to_string(version2)}

      [version] ->
        version = Hex.Version.parse!(version)
        to_string(version)
    end
  end

  defp repo(opts) do
    if organization = opts[:organization] do
      "hexpm:" <> organization
    else
      "hexpm"
    end
  end

  defp check_valid_mix_project!(package, version) do
    if is_nil(Mix.Project.get()) do
      Mix.raise(
        "Cannot execute \"mix diff #{package} #{version}\" without a Mix.Project, " <>
          "please ensure you are running Mix in a directory with a \"mix.exs\" file"
      )
    end

    loaded = Mix.Dep.Converger.converge(nil, nil, %{}, &{&1, &2, &3}) |> elem(0)

    Enum.each(loaded, fn %Mix.Dep{scm: scm, opts: opts} ->
      case scm.lock_status(opts) do
        n when n in [:mismatch, :outdated] ->
          Mix.raise(
            "The dependency is out of date. " <>
              "Please run \"deps.get\" to update \"mix.lock\" file"
          )

        _ ->
          nil
      end
    end)
  end
end
