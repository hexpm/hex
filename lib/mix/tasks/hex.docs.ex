defmodule Mix.Tasks.Hex.Docs do
  use Mix.Task

  @shortdoc "Fetches or opens documentation of a package"

  @moduledoc """
  Fetches or opens documentation of a package.

      mix hex.docs fetch PACKAGE [VERSION]

  It will retrieve and decompress the specified version of the documentation
  for a package. If you do not specify the `version` argument, this task will
  retrieve the latest documentation available in the mirror.

      mix hex.docs open PACKAGE [VERSION]

  ## Command line options

    * `--offline` - Open a local version available in your filesystem
    * `--module Some.Module` - Open a specified module documentation page inside desired package
    * `--organization ORGANIZATION` - The organization the package belongs to

  It will open the specified version of the documentation for a package in a
  Web browser. If you do not specify the `version` argument, this task will
  open the latest documentation.
  """

  @switches [offline: :boolean, module: :string, organization: :string]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      ["fetch" | remaining] ->
        fetch_docs(opts[:organization], remaining)
      ["open" | remaining] ->
        open_docs(remaining, opts)
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:

        mix hex.docs fetch PACKAGE [VERSION]
        mix hex.docs open PACKAGE [VERSION]
        """
    end
  end

  defp fetch_docs(_organization, []) do
    Mix.raise "You must specify at least the name of a package"
  end

  defp fetch_docs(organization, [name]) when name in ["eex", "elixir", "ex_unit", "iex", "logger", "mix"] do
    fetch_docs(organization, [name, System.version()])
  end

  defp fetch_docs(organization, [name]) do
    latest_version = find_package_latest_version(organization, name)
    fetch_docs(organization, [name, latest_version])
  end

  defp fetch_docs(organization, [name, version]) do
    target_dir = Path.join([docs_dir(), name, version])

    if File.exists? target_dir do
      Hex.Shell.info "Docs already fetched: #{target_dir}"
    else
      target = Path.join(target_dir, "#{name}-#{version}.tar.gz")
      retrieve_compressed_docs(organization, name, version, target)
      File.mkdir_p!(target_dir)
      extract_doc_contents(target)
      Hex.Shell.info "Docs fetched: #{target_dir}"
    end
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
        Mix.raise "No package with name #{package}"
      other ->
        Hex.Shell.error "Failed to retrieve package information"
        Hex.Utils.print_error_result(other)
    end
  end

  defp open_docs([], _opts) do
    Mix.raise "You must specify at least the name of a package"
  end

  defp open_docs(package, opts) do
    if opts[:offline] do
      open_docs_offline(package, opts)
    else
      package
      |> get_docs_url(opts)
      |> browser_open()
    end
  end

  defp open_docs_offline([name], opts) do
    {missing?, latest_version} = find_package_version(opts[:organization], name)
    if missing? do
      fetch_docs(opts[:organization], [name])
    end
    open_docs([name, latest_version], opts)
  end

  defp open_docs_offline([name, version], _opts) do
    [docs_dir(), name, version, 'index.html']
    |> Path.join()
    |> open_file()
  end

  defp find_package_version(organization, name) do
    path = Path.join(docs_dir(), name)
    if File.exists?(path) do
      {false, find_latest_version(path)}
    else
      {true, find_package_latest_version(organization, name)}
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
      System.cmd(start_command, [path])
    else
      Mix.raise "Command not found: #{start_command}"
    end
  end

  defp start_command() do
    case :os.type do
      {:win32, _} ->
        "start"
      {:unix, :darwin} ->
        "open"
      {:unix, _} ->
        "xdg-open"
    end
  end

  defp open_file(path) do
    unless File.exists?(path) do
      Mix.raise "Documentation file not found: #{path}"
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
        Mix.raise "No package with name #{package} or version #{version}"
    end
  end

  defp extract_doc_contents(target) do
    fd = File.open!(target, [:read, :compressed])
    :ok = :hex_erl_tar.extract({:file, fd}, [:compressed, cwd: Path.dirname(target)])
  end

  defp docs_dir() do
    Path.join(Hex.State.fetch!(:home), "docs")
  end
end
