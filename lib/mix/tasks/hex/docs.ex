defmodule Mix.Tasks.Hex.Docs do
  use Mix.Task

  @shortdoc "Fetch or open documentation of a package"

  @moduledoc """
  Fetch or open documentation of a package.

      mix hex.docs fetch PACKAGE [VERSION]

  It will retrieve and decompress the specified version of the documentation
  for a package. If you do not specify the `version` argument, this task will
  retrieve the documentation for version used by the current mix project or fall
  back to the latest documentation available in the mirror.

      mix hex.docs open PACKAGE [VERSION]

  ## Command line options

    * `--offline` - Open a local version available in your filesystem
    * `--module Some.Module` - Open a specified module documentation page inside desired package

  It will open the specified version of the documentation for a package in a
  Web browser. If you do not specify the `version` argument, this task will
  open the latest documentation.
  """

  @switches [offline: :boolean, module: :string]

  def run(args) do
    Hex.start
    {opts, args, _} = OptionParser.parse(args, switches: @switches)
    opts = normalize_options(opts)
    lock_deps = Mix.Project.get() && Hex.Utils.current_lock_and_deps()

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
        fetch_docs(remaining, opts, lock_deps)
      ["open" | remaining] ->
        open_docs(remaining, opts, lock_deps)
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.docs fetch PACKAGE [VERSION]
        mix hex.docs open PACKAGE [VERSION]
        """
    end
  end

  defp fetch_docs([], _opts, _lock_deps) do
    Mix.raise "You must specify at least the name of a package"
  end

  defp fetch_docs(name_version, opts, lock_deps) do
    destructure [name, version], name_version
    version = version || current_version_or_latest(name, lock_deps)
    version = resolve_version(name, version)

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

  defp resolve_version(_package, version) when is_binary(version), do: version
  defp resolve_version(package, :latest) do
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

  defp open_docs([], _opts, _lock_deps) do
    Mix.raise "You must specify at least the name of a package"
  end

  defp open_docs(package, opts, lock_deps) do
    if opts[:offline] do
      open_docs_offline(package, opts, lock_deps)
    else
      package
      |> get_docs_url(opts, lock_deps)
      |> browser_open
    end
  end

  defp open_docs_offline([name], opts, lock_deps) do
    {missing?, latest_version} = find_package_version(name, opts, lock_deps)
    if missing? do
      fetch_docs([name], opts, lock_deps)
    end
    open_docs([name, latest_version], opts, lock_deps)
  end

  defp open_docs_offline([name, version], opts, _lock_deps) do
    index_path = Path.join([opts[:home], name, version, 'index.html'])

    open_file(index_path)
  end

  defp find_package_version(name, opts, lock_deps) do
    if File.exists?("#{opts[:home]}/#{name}") do
      {false, find_latest_version("#{opts[:home]}/#{name}")}
    else
      version = current_version_or_latest(name, lock_deps)
      {true, resolve_version(name, version)}
    end
  end

  defp get_docs_url(name_version, opts, lock_deps) do
    destructure [name, version], name_version
    version = version || current_version_or_latest(name, lock_deps)
    if module = opts[:module] do
      Hex.Utils.hexdocs_module_url(name, version, module)
    else
      Hex.Utils.hexdocs_url(name, version)
    end
  end

  defp current_version_or_latest(_name, nil), do: :latest
  defp current_version_or_latest(name, {lock, _deps}) do
    case Hex.Utils.lock(lock[String.to_atom(name)]) do
      [:hex, _package, lock_version, _checksum, _managers, _deps] ->
        lock_version
      _ ->
        :latest
    end
  end

  defp browser_open(path) do
    start_browser_command =
      case :os.type do
        {:win32, _} ->
          "start"
        {:unix, :darwin} ->
          "open"
        {:unix, _} ->
          "xdg-open"
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
