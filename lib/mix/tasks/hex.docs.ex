defmodule Mix.Tasks.Hex.Docs do
  use Mix.Task

  @shortdoc "Fetches or opens documentation of a package"

  @moduledoc """
  Fetches or opens documentation of a package.

  ### Fetch documentation for offline use

  Fetches documentation for the specified package that you can later open with
  `mix hex.docs offline`.

      mix hex.docs fetch PACKAGE [VERSION]

  ### Open a browser window with offline documentation

      mix hex.docs offline PACKAGE [VERSION]

  ### Open a browser window with online documentation

      mix hex.docs online PACKAGE [VERSION]

  ## Command line options

    * `--module Some.Module` - Open a specified module documentation page inside desired package
    * `--organization ORGANIZATION` - The organization the package belongs to
  """

  @switches [module: :string, organization: :string, latest: :boolean]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)
    in_mix_project? = Hex.Utils.in_mix_project?()

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

        mix hex.docs fetch PACKAGE [VERSION]
        mix hex.docs offline PACKAGE [VERSION]
        mix hex.docs online PACKAGE [VERSION]
        """)
    end
  end

  defp fetch_docs([], _opts, true) do
    deps_in_project()
    |> Enum.each(fn pkg ->
      fetch_docs([pkg[:name], pkg[:version]], %{organization: pkg[:repo]}, true)
    end)
  end

  defp fetch_docs([], _opts, _in_mix_project?) do
    Mix.raise("You must specify at least the name of a package")
  end

  defp fetch_docs([name], opts, in_mix_project?)
       when name in ["eex", "elixir", "ex_unit", "iex", "logger", "mix"] do
    fetch_docs([name, System.version()], opts, in_mix_project?)
  end

  defp fetch_docs([name], opts, in_mix_project?) do
    locked_or_latest_version = find_package_locked_or_latest_version(opts, name, in_mix_project?)
    fetch_docs([name, locked_or_latest_version], opts, in_mix_project?)
  end

  defp fetch_docs([name, version], opts, _in_mix_project?) do
    target_dir = Path.join([docs_dir(), name, version])

    if File.exists?(target_dir) do
      Hex.Shell.info("Docs already fetched: #{target_dir}")
    else
      target = Path.join(target_dir, "#{name}-#{version}.tar.gz")
      retrieve_compressed_docs(opts[:organization], name, version, target)
      File.mkdir_p!(target_dir)
      extract_doc_contents(target)
      Hex.Shell.info("Docs fetched: #{target_dir}")
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
    Mix.raise("You must specify at least the name of a package")
  end

  defp open_docs([package, version], opts, _in_mix_project?) do
    get_docs_url([package, version], opts) |> browser_open()
  end

  defp open_docs(package, [latest: true] = opts, _in_mix_project?) do
    open_latest_docs(package, opts)
  end

  defp open_docs(package, opts, true) do
    package_in_lock =
      deps_in_project() |> Enum.find(fn pkg -> pkg[:name] == List.first(package) end)

    unless is_nil(package_in_lock) do
      version = package_in_lock[:version]
      open_docs([package, version], opts, true)
    else
      open_latest_docs(package, opts)
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
    Mix.raise("You must specify at least the name of a package")
  end

  defp open_docs_offline([name], [latest: true] = opts, in_mix_project?) do
    {missing?, latest_version} = find_package_version(opts[:organization], name)

    if missing? do
      fetch_docs([name], opts, in_mix_project?)
    end

    open_docs_offline([name, latest_version], opts, in_mix_project?)
  end

  defp open_docs_offline([name], opts, true) do
    package_in_lock =
      deps_in_project()
      |> Enum.find(fn pkg -> pkg[:name] == name end)

    unless is_nil(package_in_lock) do
      latest_version = package_in_lock[:version]
      open_docs_offline([name, latest_version], opts, true)
    else
      open_latest_docs_offline(name, opts)
    end
  end

  defp open_docs_offline([name], opts, _in_mix_project?) do
    open_latest_docs_offline(name, opts)
  end

  defp open_docs_offline([name, version], opts, in_mix_project?) do
    available? = package_version_exists?(opts[:organization], name, version)

    unless available? do
      fetch_docs([name, version], opts, in_mix_project?)
    end

    page = Keyword.get(opts, :module, "index") <> ".html"

    [docs_dir(), name, version, page]
    |> Path.join()
    |> open_file()
  end

  defp open_latest_docs_offline(name, opts) do
    {missing?, latest_version} = find_package_version(opts[:organization], name)

    if missing? do
      fetch_docs([name], opts, false)
    end

    open_docs_offline([name, latest_version], opts, true)
  end

  defp find_package_version(organization, name) do
    path = Path.join(docs_dir(), name)

    if File.exists?(path) do
      {false, find_latest_version(path)}
    else
      {true, find_package_latest_version(organization, name)}
    end
  end

  defp package_version_exists?(_organization, name, version) do
    path = Path.join([docs_dir(), name, version])
    File.exists?(path)
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
      send self(), {:hex_system_cmd, cmd, args}
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

  defp retrieve_compressed_docs(organization, package, version, target) do
    unless File.exists?(target) do
      request_docs_from_mirror(organization, package, version, target)
    end
  end

  defp request_docs_from_mirror(organization, package, version, target) do
    case Hex.Repo.get_docs(organization, package, version) do
      {:ok, {200, body, _}} ->
        File.mkdir_p!(Path.dirname(target))
        File.write!(target, body)

      _ ->
        Mix.raise("No package with name #{package} or version #{version}")
    end
  end

  defp extract_doc_contents(target) do
    fd = File.open!(target, [:read, :compressed])
    :ok = :hex_erl_tar.extract({:file, fd}, [:compressed, cwd: Path.dirname(target)])
  end

  defp docs_dir() do
    Path.join(Hex.State.fetch!(:home), "docs")
  end

  defp deps_in_project() do
    Mix.Dep.Lock.read()
    |> Enum.map(fn {_app, info} -> Hex.Utils.lock(info) end)
    |> Enum.filter(fn v -> v end)
  end
end
