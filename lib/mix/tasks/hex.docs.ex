defmodule Mix.Tasks.Hex.Docs do
  use Mix.Task

  @shortdoc "Fetches or opens documentation of a package"

  @moduledoc """
  Fetches or opens documentation of a package.

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
  """

  @switches [module: :string, organization: :string]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      ["fetch" | remaining] ->
        fetch_docs(opts[:organization], remaining)

      ["open" | _] ->
        Mix.raise("""
        Open has been removed, use one of:

        mix hex.docs online PACKAGE [VERSION]
        mix hex.docs offline PACKAGE [VERSION]
        """)

      ["online" | remaining] ->
        open_docs(remaining, opts)

      ["offline" | remaining] ->
        open_docs_offline(remaining, opts)

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.docs fetch PACKAGE [VERSION]
        mix hex.docs offline PACKAGE [VERSION]
        mix hex.docs online PACKAGE [VERSION]
        """)
    end
  end

  defp fetch_docs(_organization, []) do
    Mix.raise("You must specify at least the name of a package")
  end

  defp fetch_docs(organization, [name])
       when name in ["eex", "elixir", "ex_unit", "iex", "logger", "mix"] do
    fetch_docs(organization, [name, System.version()])
  end

  defp fetch_docs(organization, [name]) do
    latest_version = find_package_latest_version(organization, name)
    fetch_docs(organization, [name, latest_version])
  end

  defp fetch_docs(organization, [name, version]) do
    target_dir = Path.join([docs_dir(), org_dir(organization), name, version])
    fallback_dir = Path.join([docs_dir(), name, version])

    cond do
      File.exists?(target_dir) ->
        Hex.Shell.info("Docs already fetched: #{target_dir}")

      file_exists_in_fallback?(organization, name, version) ->
        Hex.Shell.info("Docs already fetched: #{fallback_dir}")

      true ->
        target = Path.join(target_dir, "#{name}-#{version}.tar.gz")
        retrieve_compressed_docs(organization, name, version, target)
        File.mkdir_p!(target_dir)
        extract_doc_contents(target)
        Hex.Shell.info("Docs fetched: #{target_dir}")
    end
  end

  defp file_exists_in_fallback?(organization, name, version)
       when is_nil(organization) do
    File.exists?(Path.join([docs_dir(), name, version]))
  end

  defp file_exists_in_fallback?(_organization, _name, _version), do: false

  defp package_exists_in_fallback?(organization, name) when is_nil(organization) do
    File.exists?(Path.join([docs_dir(), name]))
  end

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

  defp open_docs([], _opts) do
    Mix.raise("You must specify at least the name of a package")
  end

  defp open_docs(package, opts) do
    package
    |> get_docs_url(opts)
    |> browser_open()
  end

  defp open_docs_offline([], _opts) do
    Mix.raise("You must specify at least the name of a package")
  end

  defp open_docs_offline([name], opts) do
    {missing?, latest_version} = find_package_version(opts[:organization], name)

    if missing? do
      fetch_docs(opts[:organization], [name])
    end

    open_docs_offline([name, latest_version], opts)
  end

  defp open_docs_offline([name, version], opts) do
    {available?, file_location} = package_version_exists?(opts[:organization], name, version)

    unless available? do
      fetch_docs(opts[:organization], [name, version])
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

  defp org_dir(organization) when is_nil(organization), do: "hexpm"
  defp org_dir(organization), do: "hexpm:#{organization}"
end
