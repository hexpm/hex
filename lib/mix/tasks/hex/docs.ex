defmodule Mix.Tasks.Hex.Docs do
  use Mix.Task

  @shortdoc "Fetch or open documentation of a package"

  @moduledoc """
  Fetch or open documentation of a package

      mix hex.docs fetch PACKAGE [VERSION]

  It will retrieve and decompress the specified version of the documentation
  for a package. If you do not specify the `version` argument, this task will
  retrieve the latest documentation available in the mirror.

      mix hex.docs open PACKAGE [VERSION]

  ## Command line options

    * `--offline` - Open a local version available in your filesystem

  It will open the specified version of the documentation for a package in a
  Web browser. If you do not specify the `version` argument, this task will
  open the latest documentation.
  """

  @switches [offline: :boolean]

  def run(args) do
    Hex.start
    {opts, args, _} = OptionParser.parse(args, switches: @switches)
    opts = normalize_options(opts)

    case args do
      [] ->
        Mix.raise """
        [deprecation] The "mix hex.docs" command has changed. To use the old
        behaviour (publishing docs), use:

            mix hex.publish docs

        The new "mix hex.docs" command has to be invoked with at least one
        argument. Call "mix help hex.docs" for more information.
        """
      ["fetch" | remaining] ->
        fetch_docs(remaining, opts)
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

  defp fetch_docs([], _opts) do
    Mix.raise "You must specify at least the name of a package"
  end

  defp fetch_docs([name], opts) do
    latest_version = find_package_latest_version(name)
    fetch_docs([name, latest_version], opts)
  end

  defp fetch_docs([name, version], opts) do
    target_dir = Path.join([opts[:home], name, version])

    if File.exists? target_dir do
      Hex.Shell.info "Docs already fetched: #{target_dir}"
    else
      doc_filename = "#{name}-#{version}.tar.gz"
      doc_url = Hex.API.repo_url("docs/#{doc_filename}")
      retrieve_compressed_docs(doc_url, doc_filename, opts)
      File.mkdir_p! target_dir
      extract_doc_contents(doc_filename, target_dir, opts)
      Hex.Shell.info "Docs fetched: #{target_dir}"
    end
  end

  defp find_package_latest_version(package) do
    %{"releases" => releases} = retrieve_package_info(package)

    latest_release =
      releases
      |> Enum.sort(&(Hex.Version.compare(&1["version"], &2["version"]) == :gt))
      |> List.first()

    latest_release["version"]
 end

  defp retrieve_package_info(package) do
    case Hex.API.Package.get(package) do
      {code, body, _} when code in 200..299 ->
        body
      {404, _, _} ->
        Mix.raise "No package with name #{package}"
      {code, body, _} ->
        Hex.Shell.error "Failed to retrieve package information"
        Hex.Utils.print_error_result(code, body)
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
      |> get_docs_url
      |> browser_open
    end
  end

  defp open_docs_offline([name], opts) do
    {missing?, latest_version} = find_package_version(name, opts)
    if missing? do
      fetch_docs([name], opts)
    end
    open_docs([name, latest_version], opts)
  end

  defp open_docs_offline([name, version], opts) do
    index_path = Path.join([opts[:home], name, version, 'index.html'])

    open_file(index_path)
  end

  defp find_package_version(name, opts) do
    if File.exists?("#{opts[:home]}/#{name}") do
      {false, find_latest_version("#{opts[:home]}/#{name}")}
    else
      {true, find_package_latest_version(name)}
    end
  end

  defp get_docs_url([name]) do
    Hex.Utils.hexdocs_url(name)
  end

  defp get_docs_url([name, version]) do
    Hex.Utils.hexdocs_url(name, version)
  end

  defp browser_open(path) do
    start_browser_command =
      case :os.type do
        {:win32, _} ->
          "start"
        {:unix, _} ->
          "open"
      end

    if System.find_executable(start_browser_command) do
      System.cmd(start_browser_command, [path])
    else
      Mix.raise "Command not found: #{start_browser_command}"
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
    |> File.ls!
    |> Enum.sort(&(Hex.Version.compare(&1, &2) == :gt))
    |> List.first
  end

  defp retrieve_compressed_docs(url, filename, opts) do
    target = Path.join(opts[:cache], filename)
    File.mkdir_p!(opts[:cache])

    unless File.exists?(target) do
      request_docs_from_mirror(url, target)
    end
  end

  defp request_docs_from_mirror(url, target) do
    {:ok, body, _} = Hex.Repo.request(url, nil)
    File.write!(target, body)
  end

  defp extract_doc_contents(filename, target_dir, opts) do
    target_file = Path.join(opts[:cache], filename)
    fd = File.open!(target_file, [:read, :compressed])
    Hex.Tar.extract_contents(fd, target_dir, [mode: :file])
  end

  defp normalize_options(opts) do
    home = Hex.State.fetch!(:home)
    docs_root = Path.join(home, "docs")
    cache_dir = Path.join(docs_root, ".cache")

    opts
    |> Keyword.put(:home, docs_root)
    |> Keyword.put(:cache, cache_dir)
  end
end
