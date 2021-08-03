defmodule Mix.Tasks.Hex.Package do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Fetches or diffs packages"

  @default_diff_command "git diff --no-index __PATH1__ __PATH2__"

  @doc false
  def default_diff_command(), do: @default_diff_command

  @moduledoc """
  Fetches or diffs packages.

  ## Fetch package

  Fetch a package tarball to the current directory.

      $ mix hex.package fetch PACKAGE [VERSION] [--unpack] [--output PATH]

  If `version` is not given, use the latest version.

  You can pipe the fetched tarball to stdout by setting `--output -`.

  ## Diff package versions

      $ mix hex.package diff APP VERSION

  This command compares the project's dependency `APP` against
  the target package version, unpacking the target version into
  temporary directory and running a diff command.

  ## Fetch and diff package contents between versions

      $ mix hex.package diff PACKAGE VERSION1 VERSION2
      $ mix hex.package diff PACKAGE VERSION1..VERSION2

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

      $ #{@default_diff_command}

  The `__PATH1__` and `__PATH2__` placeholders will be interpolated with
  paths to directories of unpacked tarballs for each version.

  Many diff commands supports coloured output but because we execute
  the command in non-interactive mode, they'd usually be disabled.

  On Unix systems you can pipe the output to more commands, for example:

      $ mix hex.package diff decimal 1.0.0..1.1.0 | colordiff | less -R

  Here, the output of `mix hex.package diff` is piped to the `colordiff`
  utility to adds colours, which in turn is piped to `less -R` which
  "pages" it. (`-R` preserves escape codes which allows colours to work.)

  Another option is to configure the diff command itself. For example, to
  force Git to always colour the output we can set the `--color=always` option:

      $ mix hex.config diff_command "git diff --color=always --no-index __PATH1__ __PATH2__"
      $ mix hex.package diff decimal 1.0.0..1.1.0

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
      ["fetch", package] ->
        fetch(repo(opts), package, nil, unpack, output)

      ["fetch", package, version] ->
        fetch(repo(opts), package, version, unpack, output)

      ["diff", package, version1, version2] ->
        diff(repo(opts), package, parse_version!(version1, version2))

      ["diff", package, version] ->
        diff(repo(opts), package, parse_version!(version))

      _ ->
        Mix.raise("""
          Invalid arguments, expected one of:

          mix hex.package fetch PACKAGE [VERSION] [--unpack]
          mix hex.package diff APP VERSION
          mix hex.package diff PACKAGE VERSION1 VERSION2
          mix hex.package diff PACKAGE VERSION1..VERSION2
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"fetch PACKAGE [VERSION] [--unpack]", "Fetch the package"},
      {"diff APP VERSION", "Diff dependency against version"},
      {"diff PACKAGE VERSION1 VERSION2", "Diff package versions"},
      {"diff PACKAGE VERSION1..VERSION2", "Diff package versions"}
    ]
  end

  defp fetch(repo, package, nil, unpack?, output) do
    version = find_package_latest_version(repo, package)
    fetch(repo, package, version, unpack?, output)
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
    if output, do: File.mkdir_p!(output)

    abs_name = Path.absname("#{package}-#{version}")

    {abs_path, tar_path} =
      if output do
        {output, Path.join(output, "#{package}-#{version}.tar")}
      else
        {abs_name, "#{abs_name}.tar"}
      end

    File.write!(tar_path, tarball)

    if unpack? do
      %{inner_checksum: inner_checksum, outer_checksum: outer_checksum} =
        Hex.Tar.unpack!(tar_path, abs_path)

      verify_inner_checksum!(repo, package, version, inner_checksum)
      verify_outer_checksum!(repo, package, version, outer_checksum)
    else
      {:ok, outer_checksum} = Hex.Tar.outer_checksum(tar_path)
      verify_outer_checksum!(repo, package, version, outer_checksum)
    end

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
    path = Hex.SCM.cache_path(repo, package, version)

    case Hex.SCM.fetch(repo, package, version) do
      {:ok, _} ->
        File.read!(path)

      {:error, reason} ->
        if File.exists?(path) do
          File.read!(path)
        else
          Mix.raise(
            "Downloading " <>
              Hex.Repo.tarball_url(repo, package, version) <> " failed:\n\n" <> reason
          )
        end
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

  defp diff(repo, app, version) when is_binary(version) do
    Hex.Mix.check_deps()

    {path_lock, package} =
      case Map.get(Mix.Dep.Lock.read(), String.to_atom(app)) do
        nil ->
          Mix.raise(
            "Cannot find the app \"#{app}\" in \"mix.lock\" file, " <>
              "please ensure it has been specified in \"mix.exs\" and run \"mix deps.get\""
          )

        lock ->
          path = Path.join(Mix.Project.deps_path(), app)
          package = Hex.Utils.lock(lock).name
          {path, package}
      end

    path = tmp_path("#{package}-#{version}-")

    try do
      fetch_and_unpack!(repo, package, [{path, version}])
      code = run_diff_path!(path_lock, path)
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
      code = run_diff_path!(path1, path2)
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
          Hex.Tar.unpack!({:binary, tarball}, path)

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
      |> String.replace("__PATH1__", escape_and_quote_path(path1))
      |> String.replace("__PATH2__", escape_and_quote_path(path2))

    Mix.shell().cmd(cmd)
  end

  defp escape_and_quote_path(path) do
    escaped = String.replace(path, "\"", "\\\"")
    ~s("#{escaped}")
  end

  defp tmp_path(prefix) do
    random_string = Base.encode16(:crypto.strong_rand_bytes(4))
    Path.join(System.tmp_dir!(), prefix <> random_string)
  end

  defp parse_version!(string) do
    case String.split(string, "..", trim: true) do
      [version1, version2] ->
        parse_two_versions!(version1, version2)

      [version] ->
        version |> Hex.Version.parse!() |> to_string()
    end
  end

  defp parse_version!(version1, version2) do
    parse_two_versions!(version1, version2)
  end

  defp parse_two_versions!(version1, version2) do
    version1 = Hex.Version.parse!(version1)
    version2 = Hex.Version.parse!(version2)
    {to_string(version1), to_string(version2)}
  end

  defp repo(opts) do
    repo = Keyword.get(opts, :repo, "hexpm")

    if organization = opts[:organization] do
      Enum.join([repo, organization], ":")
    else
      repo
    end
  end

  defp find_package_latest_version(organization, name) do
    %{"latest_stable_version" => latest_stable_version} =
      retrieve_package_info(organization, name)

    latest_stable_version
  end

  defp retrieve_package_info(organization, name) do
    case Hex.API.Package.get(organization, name) do
      {:ok, {code, body, _}} when code in 200..299 ->
        body

      {:ok, {404, _, _}} ->
        Mix.raise("No package with name #{name}")

      other ->
        Hex.Shell.error("Failed to retrieve package information")
        Hex.Utils.print_error_result(other)
    end
  end
end
