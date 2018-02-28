defmodule Mix.Tasks.Hex.Docs do
  use Mix.Task

  @shortdoc "Fetches or opens documentation of a package"

  @moduledoc """
  Fetches or opens documentation of a package.

  If no version is specified, defaults to version used in the current mix project.
  If called outside of a mix project or the dependency is not used in the
  current mix project, defaults to the latest version.

  ## Fetch documentation for all dependencies in the current mix project

    mix hex.docs fetch

  ## Fetch documentation for offline use

  Fetches documentation for the specified package that you can later open with
  `mix hex.docs offline`.

      mix hex.docs fetch PACKAGE [VERSION]

  ## Open a browser window with offline documentation

      mix hex.docs offline PACKAGE [VERSION]

  ## Open a browser window with online documentation

      mix hex.docs online PACKAGE [VERSION]

  ## Command line options

    * `--module Some.Module` - Open a specified module documentation page inside desired package
    * `--organization ORGANIZATION` - The organization the package belongs to
    * `--latest` - Looks for the latest release of a package
  """

  @switches [module: :string, organization: :string, latest: :boolean]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)
    in_mix_project? = !!Mix.Project.get()

    case args do
      ["fetch" | remaining] ->
        fetch_docs(remaining, opts, in_mix_project?)

      ["open" | _] ->
        Mix.raise("""
        Open has been removed, use one of:

        mix hex.docs online PACKAGE [VERSION]
        mix hex.docs offline PACKAGE [VERSION]
        """)

      ["online" | remaining] ->
        open_docs(remaining, opts, in_mix_project?)

      ["offline" | remaining] ->
        open_docs_offline(remaining, opts, in_mix_project?)

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

  defp fetch_docs([], _opts, true = _in_mix_project?) do
    Enum.each(deps_in_project(), fn pkg ->
      fetch_docs([pkg[:name], pkg[:version]], %{organization: pkg[:repo]}, true, true)
    end)
  end

  defp fetch_docs([], _opts, false = _in_mix_project?) do
    Mix.raise(
      "Specify a package name or run inside a Mix project to fetch docs for all dependencies"
    )
  end

  defp fetch_docs([name], opts, in_mix_project?)
       when name in ["eex", "elixir", "ex_unit", "iex", "logger", "mix"] do
    fetch_docs([name, System.version()], opts, in_mix_project?)
  end

  defp fetch_docs([name], opts, in_mix_project?) do
    locked_or_latest_version = find_package_locked_or_latest_version(opts, name, in_mix_project?)
    fetch_docs([name, locked_or_latest_version], opts, in_mix_project?)
  end

  defp fetch_docs([name, version], opts, _in_mix_project?, continue_on_error? \\ false) do
    target_dir = Path.join([docs_dir(), org_dir(opts[:organization]), name, version])
    fallback_dir = Path.join([docs_dir(), name, version])

    cond do
      File.exists?(target_dir) ->
        Hex.Shell.info("Docs already fetched: #{target_dir}")

      file_exists_in_fallback?(opts[:organization], name, version) ->
        Hex.Shell.info("Docs already fetched: #{fallback_dir}")

      true ->
        target = Path.join(target_dir, "#{name}-#{version}.tar.gz")

        {_resp, retrieved?} =
          retrieve_compressed_docs(opts[:organization], name, version, target, continue_on_error?)

        make_dir_and_extract(target, target_dir, retrieved?)
    end
  end

  defp find_package_locked_or_latest_version([latest: true] = opts, package, _in_mix_project?) do
    find_package_latest_version(opts[:organization], package)
  end

  defp find_package_locked_or_latest_version(opts, package, true) do
    package_in_lock =
      deps_in_project()
      |> Enum.find(fn pkg -> pkg[:name] == package end)

    if is_nil(package_in_lock) do
      find_package_latest_version(opts[:organization], package)
    else
      package_in_lock[:version]
    end
  end

  defp find_package_locked_or_latest_version(opts, package, _in_mix_project?) do
    find_package_latest_version(opts[:organization], package)
  end

  defp file_exists_in_fallback?(nil, name, version),
    do: File.exists?(Path.join([docs_dir(), name, version]))

  defp file_exists_in_fallback?(_organization, _name, _version), do: false

  defp package_exists_in_fallback?(nil, name), do: File.exists?(Path.join([docs_dir(), name]))
  defp package_exists_in_fallback?(_organization, _name), do: false

  defp find_package_latest_version(organization, package) do
    %{"releases" => releases} = retrieve_package_info(organization, package)

    latest_release =
      releases
      |> Enum.sort(&(Hex.Version.compare(&1["version"], &2["version"]) == :gt))
      |> List.first()

    latest_release["version"]
  end

  defp retrieve_package_info(organization, package) do
    case Hex.API.Package.get(organization, package) do
      {:ok, {code, body, _}} when code in 200..299 ->
        body

      {:ok, {404, _, _}} ->
        Mix.raise("No package with name #{package}")

      other ->
        Hex.Shell.error("Failed to retrieve package information")
        Hex.Utils.print_error_result(other)
    end
  end

  defp open_docs([], _opts, _in_mix_project?) do
    Mix.raise("You must specify the name of a package")
  end

  defp open_docs([package, version], opts, _in_mix_project?) do
    get_docs_url([package, version], opts) |> browser_open()
  end

  defp open_docs(package, [latest: true] = opts, _in_mix_project?) do
    open_latest_docs(package, opts)
  end

  defp open_docs(package, opts, true) do
    package_in_lock =
      Enum.find(deps_in_project(), fn pkg -> pkg[:name] == List.first(package) end)

    if is_nil(package_in_lock) do
      open_latest_docs(package, opts)
    else
      version = package_in_lock[:version]
      open_docs([package, version], opts, true)
    end
  end

  defp open_docs(package, opts, _in_mix_project?) do
    open_latest_docs(package, opts)
  end

  defp open_latest_docs(package, opts) do
    package
    |> get_docs_url(opts)
    |> browser_open()
  end

  defp open_docs_offline([], _opts, _in_mix_project?) do
    Mix.raise("You must specify the name of a package")
  end

  defp open_docs_offline([name], [latest: true] = opts, in_mix_project?) do
    {missing?, latest_version} = find_package_version(opts[:organization], name)

    if missing? do
      fetch_docs([name], opts, in_mix_project?)
    end

    open_docs_offline([name, latest_version], opts, in_mix_project?)
  end

  defp open_docs_offline([name], opts, true) do
    package_in_lock = Enum.find(deps_in_project(), fn pkg -> pkg[:name] == name end)

    if is_nil(package_in_lock) do
      open_latest_docs_offline(name, opts)
    else
      latest_version = package_in_lock[:version]
      open_docs_offline([name, latest_version], opts, true)
    end
  end

  defp open_docs_offline([name], opts, _in_mix_project?) do
    open_latest_docs_offline(name, opts)
  end

  defp open_docs_offline([name, version], opts, in_mix_project?) do
    {available?, file_location} = package_version_exists?(opts[:organization], name, version)

    unless available? do
      fetch_docs([name, version], opts, in_mix_project?)
    end

    page = Keyword.get(opts, :module, "index") <> ".html"

    available_doc_path =
      if file_location == :fallback do
        Path.join([docs_dir(), name, version, page])
      else
        Path.join([docs_dir(), org_dir(opts[:organization]), name, version, page])
      end

    open_file(available_doc_path)
  end

  defp open_latest_docs_offline(name, opts) do
    {missing?, latest_version} = find_package_version(opts[:organization], name)

    if missing? do
      fetch_docs([name], opts, false)
    end

    open_docs_offline([name, latest_version], opts, true)
  end

  defp find_package_version(organization, name) do
    path = Path.join([docs_dir(), org_dir(organization), name])
    fallback_path = Path.join([docs_dir(), name])

    cond do
      File.exists?(path) ->
        {false, find_latest_version(path)}

      package_exists_in_fallback?(organization, name) ->
        {false, find_latest_version(fallback_path)}

      true ->
        {true, find_package_latest_version(organization, name)}
    end
  end

  defp package_version_exists?(organization, name, version) do
    path = Path.join([docs_dir(), org_dir(organization), name, version])

    cond do
      File.exists?(path) -> {true, :default}
      file_exists_in_fallback?(organization, name, version) -> {true, :fallback}
      true -> {false, :default}
    end
  end

  defp get_docs_url([name], opts) do
    if module = opts[:module] do
      Hex.Utils.hexdocs_module_url(name, module)
    else
      Hex.Utils.hexdocs_url(name)
    end
  end

  defp get_docs_url([name, version], opts) do
    if module = opts[:module] do
      Hex.Utils.hexdocs_module_url(name, version, module)
    else
      Hex.Utils.hexdocs_url(name, version)
    end
  end

  defp browser_open(path) do
    start_command = start_command()

    if System.find_executable(start_command) do
      system_cmd(start_command, [path])
    else
      Mix.raise("Command not found: #{start_command}")
    end
  end

  defp start_command() do
    case :os.type() do
      {:win32, _} -> "start"
      {:unix, :darwin} -> "open"
      {:unix, _} -> "xdg-open"
    end
  end

  if Mix.env() == :test do
    defp system_cmd(cmd, args) do
      send(self(), {:hex_system_cmd, cmd, args})
    end
  else
    defp system_cmd(cmd, args) do
      System.cmd(cmd, args)
    end
  end

  defp open_file(path) do
    unless File.exists?(path) do
      Mix.raise("Documentation file not found: #{path}")
    end

    browser_open(path)
  end

  defp find_latest_version(path) do
    path
    |> File.ls!()
    |> Enum.sort(&(Hex.Version.compare(&1, &2) == :gt))
    |> List.first()
  end

  defp retrieve_compressed_docs(organization, package, version, target, continue_on_error?) do
    unless File.exists?(target) do
      request_docs_from_mirror(organization, package, version, target, continue_on_error?)
    end
  end

  defp request_docs_from_mirror(organization, package, version, target, continue_on_error?) do
    case Hex.Repo.get_docs(organization, package, version) do
      {:ok, {200, body, _}} ->
        File.mkdir_p!(Path.dirname(target))
        File.write!(target, body)
        {:ok, true}

      _ ->
        if continue_on_error? do
          Hex.Shell.info(
            "Couldn't find docs for package with name #{package} or version #{version}"
          )
        else
          Mix.raise("Couldn't find docs for package with name #{package} or version #{version}")
        end

        {:error, false}
    end
  end

  defp make_dir_and_extract(target, target_dir, retrieved?) do
    if retrieved? do
      File.mkdir_p!(target_dir)
      extract_doc_contents(target)
      Hex.Shell.info("Docs fetched: #{target_dir}")
    end
  end

  defp extract_doc_contents(target) do
    fd = File.open!(target, [:read, :compressed])
    :ok = :vendored_hex_erl_tar.extract({:file, fd}, [:compressed, cwd: Path.dirname(target)])
  end

  defp docs_dir() do
    Path.join(Hex.State.fetch!(:home), "docs")
  end

  defp deps_in_project() do
    Mix.Dep.Lock.read()
    |> Enum.map(fn {_app, info} -> Hex.Utils.lock(info) end)
    |> Enum.reject(&is_nil/1)
  end

  defp org_dir(organization) when organization in [nil, "hexpm"], do: "hexpm"
  defp org_dir(organization), do: "hexpm:#{organization}"
end
