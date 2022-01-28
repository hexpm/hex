defmodule Mix.Tasks.Hex.Docs do
  use Mix.Task

  @shortdoc "Fetches or opens documentation of a package"

  @moduledoc """
  Fetches or opens documentation of a package.

  If no version is specified, defaults to version used in the current mix project.
  If called outside of a mix project or the dependency is not used in the
  current mix project, defaults to the latest version.

  ## Fetch documentation for all dependencies in the current mix project

      $ mix hex.docs fetch

  ## Fetch documentation for offline use

  Fetches documentation for the specified package that you can later open with
  `mix hex.docs offline`.

      $ mix hex.docs fetch PACKAGE [VERSION]

  ## Open a browser window with offline documentation

      $ mix hex.docs offline PACKAGE [VERSION]

  ## Open a browser window with online documentation

      $ mix hex.docs online PACKAGE [VERSION]

  ## Command line options

    * `--module Some.Module` - Open a specified module documentation page inside desired package
    * `--organization ORGANIZATION` - Set this for private packages belonging to an organization
    * `--latest` - Looks for the latest release of a package
    * `--format epub` - When opening documentation offline, use this flag to open the epub formatted version
  """
  @behaviour Hex.Mix.TaskDescription

  @elixir_apps ~w(eex elixir ex_unit iex logger mix)
  @switches [module: :string, organization: :string, latest: :boolean, format: :string]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)
    opts = Keyword.put(opts, :mix_project, !!Mix.Project.get())

    case args do
      ["fetch" | remaining] ->
        fetch_docs(remaining, opts)

      ["online" | remaining] ->
        open_docs(remaining, opts)

      ["offline" | remaining] ->
        open_docs_offline(remaining, opts)

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.docs fetch
        mix hex.docs fetch PACKAGE [VERSION]
        mix hex.docs offline PACKAGE [VERSION]
        mix hex.docs online PACKAGE [VERSION]
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"fetch PACKAGE [VERSION]", "Fetch documentation for offline use"},
      {"offline PACKAGE [VERSION]", "Open a browser window with offline documentation"},
      {"online PACKAGE [VERSION]", "Open a browser window with online documentation"}
    ]
  end

  defp fetch_docs([] = _args, opts) do
    if !opts[:mix_project] do
      Mix.raise(
        "Specify a package name or run inside a Mix project " <>
          "to fetch docs for all dependencies"
      )
    end

    Enum.each(deps_in_lock(), fn package ->
      fetch_docs([package.name, package.version], organization: package.repo)
    end)
  end

  defp fetch_docs([name], opts) when name in @elixir_apps do
    fetch_docs([name, System.version()], opts)
  end

  defp fetch_docs([name], opts) do
    locked_or_latest_version = find_package_locked_or_latest_version(name, opts)
    fetch_docs([name, locked_or_latest_version], opts)
  end

  defp fetch_docs([name, version], opts) do
    target_dir = Path.join([docs_dir(), org_to_path(opts[:organization]), name, version])
    fallback_dir = Path.join([docs_dir(), name, version])

    cond do
      File.exists?(target_dir) ->
        Hex.Shell.info("Docs already fetched: #{target_dir}")

      File.exists?(fallback_dir) ->
        Hex.Shell.info("Docs already fetched: #{fallback_dir}")

      true ->
        target = Path.join(target_dir, "#{name}-#{version}.tar.gz")
        success? = download_docs(opts[:organization], name, version, target)

        if success? do
          extract_docs(target, target_dir)
        end
    end
  end

  defp find_package_locked_or_latest_version(name, opts) do
    package_in_lock = package_in_lock(name)

    if opts[:mix_project] && !opts[:latest] && package_in_lock do
      package_in_lock.version
    else
      find_package_latest_version(opts[:organization], name)
    end
  end

  defp find_package_latest_version(organization, name) do
    %{"releases" => releases} = retrieve_package_info(organization, name)

    sorted_versions =
      Enum.sort(releases, &(Hex.Version.compare(&1["version"], &2["version"]) == :gt))

    if Enum.all?(sorted_versions, &pre_release?/1) do
      sorted_versions
      |> List.first()
      |> Map.get("version")
    else
      sorted_versions
      |> Enum.reject(&pre_release?/1)
      |> List.first()
      |> Map.get("version")
    end
  end

  defp retrieve_package_info(organization, name) do
    auth = if organization, do: Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.Package.get(organization, name, auth) do
      {:ok, {code, body, _}} when code in 200..299 ->
        body

      {:ok, {404, _, _}} ->
        Mix.raise("No package with name #{name}")

      other ->
        Hex.Shell.error("Failed to retrieve package information")
        Hex.Utils.print_error_result(other)
    end
  end

  defp open_docs([] = _args, _opts) do
    Mix.raise("You must specify the name of a package")
  end

  defp open_docs([name], opts) do
    package_in_lock = package_in_lock(name)

    if opts[:mix_project] && !opts[:latest] && package_in_lock do
      version = package_in_lock.version
      open_docs([name, version], opts)
    else
      open_latest_docs([name], opts)
    end
  end

  defp open_docs([name, version], opts) do
    get_docs_url([name, version], opts)
    |> browser_open()
  end

  defp open_latest_docs(args, opts) do
    args
    |> get_docs_url(opts)
    |> browser_open()
  end

  defp open_docs_offline([] = _args, _opts) do
    Mix.raise("You must specify the name of a package")
  end

  defp open_docs_offline([name], opts) do
    package_in_lock = package_in_lock(name)

    if opts[:mix_project] && !opts[:latest] && package_in_lock do
      latest_version = package_in_lock.version
      open_docs_offline([name, latest_version], opts)
    else
      open_latest_docs_offline(name, opts)
    end
  end

  defp open_docs_offline([name, version], opts) do
    docs_location = docs_location(opts[:organization], name, version, opts)

    if docs_location do
      open_file(docs_location)
    else
      fetch_docs([name, version], opts)
      docs_location = docs_location(opts[:organization], name, version, opts)

      if docs_location do
        open_file(docs_location)
      end
    end
  end

  defp open_latest_docs_offline(name, opts) do
    latest_version = find_package_version(opts[:organization], name)

    if latest_version do
      open_docs_offline([name, latest_version], opts)
    else
      fetch_docs([name], opts)
      latest_version = find_package_version(opts[:organization], name)

      if latest_version do
        open_docs_offline([name, latest_version], opts)
      end
    end
  end

  defp docs_location(organization, name, version, opts) do
    format = Keyword.get(opts, :format, "html")
    module = Keyword.get(opts, :module, "index")

    default_path = Path.join([docs_dir(), org_to_path(organization), name, version])
    fallback_path = Path.join([docs_dir(), name, version])

    case format do
      "epub" -> epub_file_location(default_path, fallback_path, organization)
      "html" -> html_file_location(default_path, fallback_path, module, organization)
    end
  end

  defp html_file_location(default_path, fallback_path, module, organization) do
    default_path = Path.join([default_path, module <> ".html"])
    fallback_path = Path.join([fallback_path, module <> ".html"])

    cond do
      File.exists?(default_path) -> default_path
      !organization && File.exists?(fallback_path) -> fallback_path
      true -> nil
    end
  end

  defp epub_file_location(default_path, fallback_path, organization) do
    default_path = Path.wildcard(Path.join([default_path, "*.epub"]))
    fallback_path = Path.wildcard(Path.join([fallback_path, "*.epub"]))

    cond do
      length(default_path) == 1 -> Enum.at(default_path, 0)
      !organization && length(fallback_path) == 1 -> Enum.at(fallback_path, 0)
      true -> Mix.raise("No documentation found in epub format.")
    end
  end

  defp find_package_version(organization, name) do
    default_path = Path.join([docs_dir(), org_to_path(organization), name])
    fallback_path = Path.join([docs_dir(), name])

    cond do
      File.exists?(default_path) -> find_latest_version(default_path)
      !organization && File.exists?(fallback_path) -> find_latest_version(fallback_path)
      true -> nil
    end
  end

  defp get_docs_url([name], opts) do
    if module = opts[:module] do
      Hex.Utils.hexdocs_module_url(opts[:organization], name, module)
    else
      Hex.Utils.hexdocs_url(opts[:organization], name)
    end
  end

  defp get_docs_url([name, version], opts) do
    if module = opts[:module] do
      Hex.Utils.hexdocs_module_url(opts[:organization], name, version, module)
    else
      Hex.Utils.hexdocs_url(opts[:organization], name, version)
    end
  end

  defp browser_open(path) do
    path
    |> open_cmd()
    |> system_cmd()
  end

  defp open_cmd(path) do
    case :os.type() do
      {:win32, _} ->
        dirname = Path.dirname(path)
        basename = Path.basename(path)
        {"cmd", ["/c", "start", basename], [cd: dirname]}

      {:unix, :darwin} ->
        {"open", [path], []}

      {:unix, _} ->
        {"xdg-open", [path], []}
    end
  end

  if Mix.env() == :test do
    defp system_cmd({cmd, args, options}) do
      send(self(), {:hex_system_cmd, cmd, args, options})
    end
  else
    defp system_cmd({cmd, args, options}) do
      System.cmd(cmd, args, options)
    end
  end

  defp open_file(path) do
    unless path do
      Mix.raise("Documentation not found")
    end

    unless File.exists?(path) do
      Mix.raise("Documentation file not found: #{path}")
    end

    browser_open(path)
  end

  defp find_latest_version(path) do
    sorted_versions =
      path
      |> File.ls!()
      |> Enum.sort(&(Hex.Version.compare(&1, &2) == :gt))

    if Enum.all?(sorted_versions, &pre_release?/1) do
      List.first(sorted_versions)
    else
      sorted_versions
      |> Enum.reject(&pre_release?/1)
      |> List.first()
    end
  end

  defp download_docs(organization, package, version, target) do
    repo = org_to_repo(organization)

    case Hex.Repo.get_docs(repo, package, version) do
      {:ok, {200, body, _}} ->
        File.mkdir_p!(Path.dirname(target))
        File.write!(target, body)
        true

      _ ->
        message = "Couldn't find docs for package with name #{package} or version #{version}"
        Hex.Shell.error(message)
        false
    end
  end

  defp extract_docs(target, target_dir) do
    File.mkdir_p!(target_dir)
    fd = File.open!(target, [:read, :compressed])
    :ok = :mix_hex_erl_tar.extract({:file, fd}, [:compressed, cwd: Path.dirname(target)])
    Hex.Shell.info("Docs fetched: #{target_dir}")
  end

  defp docs_dir() do
    Path.join(Hex.State.fetch!(:data_home), "docs")
  end

  defp package_in_lock(name) do
    Enum.find(deps_in_lock(), &(&1.name == name))
  end

  defp deps_in_lock() do
    Mix.Dep.Lock.read()
    |> Enum.map(fn {_app, info} -> Hex.Utils.lock(info) end)
    |> Enum.reject(&is_nil/1)
  end

  defp org_to_repo(organization) when organization in [nil, "hexpm"], do: "hexpm"
  defp org_to_repo(organization), do: "hexpm:#{organization}"

  defp org_to_path(organization) do
    organization
    |> org_to_repo()
    |> Hex.Utils.windows_repo_path_fix()
  end

  defp pre_release?(%{"version" => version}), do: do_pre_release?(version)
  defp pre_release?(version), do: do_pre_release?(version)

  defp do_pre_release?(version) do
    case Hex.Version.parse(version) do
      {:ok, %Version{pre: []}} -> false
      {:ok, %Version{}} -> true
      _ -> false
    end
  end
end
