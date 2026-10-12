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

    * `--page "Some.Module"`, `--page guide` - Open a specified documentation page inside desired package
    * `--organization ORGANIZATION` - Set this for private packages belonging to an organization
    * `--latest` - Looks for the latest release of a package
    * `--format epub`, `--format md` - When opening documentation offline, use this flag to open the epub/md formatted version
  """
  @behaviour Hex.Mix.TaskDescription

  @elixir_apps ~w(eex elixir ex_unit iex logger mix)
  # TODO: Remove :module switch
  @switches [
    module: :string,
    page: :string,
    organization: :string,
    latest: :boolean,
    format: :string
  ]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = OptionParser.parse!(args, strict: @switches)
    opts = opts |> normalize_module_opt() |> Keyword.put(:mix_project, !!Mix.Project.get())

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
      case repo_to_org(package.repo) do
        {:ok, organization} ->
          fetch_docs([package.name, package.version], organization: organization)

        :error ->
          Hex.Shell.warn(
            "Skipping docs for #{package.name} #{package.version}, docs can only be " <>
              "fetched from hexpm and its organizations, not from #{package.repo}"
          )
      end
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
    validate_package!(opts[:organization], name)
    validate_version!(version)

    target_dir = Path.join([docs_dir(), org_to_path(opts[:organization]), name, version])
    fallback_dir = Path.join([docs_dir(), name, version])

    cond do
      File.exists?(target_dir) ->
        Hex.Shell.info("Docs already fetched: #{target_dir}")

      File.exists?(fallback_dir) ->
        Hex.Shell.info("Docs already fetched: #{fallback_dir}")

      true ->
        # The docs are unpacked into a new directory that is renamed to
        # target_dir, so a failed fetch only removes what it created
        random = Base.encode16(:crypto.strong_rand_bytes(8), case: :lower)
        tmp_dir = Path.join(docs_dir(), ".#{name}-#{version}.#{random}")
        target = Path.join(tmp_dir, "#{name}-#{version}.tar.gz")

        try do
          if download_docs(opts[:organization], name, version, target) do
            extract_docs(target, tmp_dir, target_dir)
          end
        after
          File.rm_rf(tmp_dir)
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

    sorted_versions = Enum.sort(releases, &(Version.compare(&1["version"], &2["version"]) == :gt))

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
    case Hex.API.Package.get(organization, name) do
      {:ok, {code, _, body}} when code in 200..299 ->
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
    validate_package!(opts[:organization], name)
    validate_version!(version)
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
    validate_package!(opts[:organization], name)
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
    page = Keyword.get(opts, :page)

    default_path = Path.join([docs_dir(), org_to_path(organization), name, version])
    fallback_path = Path.join([docs_dir(), name, version])

    case format do
      "epub" ->
        epub_file_location(default_path, fallback_path, organization)

      "html" ->
        file_location(default_path, fallback_path, "#{page || "index"}.html", organization)

      "md" when page == nil ->
        file_location(default_path, fallback_path, "llms.txt", organization)

      "md" ->
        file_location(default_path, fallback_path, "#{page}.md", organization)
    end
  end

  defp file_location(default_path, fallback_path, page, organization) do
    default_path = Path.join([default_path, page])
    fallback_path = Path.join([fallback_path, page])

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
    if page = opts[:page] do
      Hex.Utils.hexdocs_module_url(opts[:organization], name, page)
    else
      Hex.Utils.hexdocs_url(opts[:organization], name)
    end
  end

  defp get_docs_url([name, version], opts) do
    if page = opts[:page] do
      Hex.Utils.hexdocs_module_url(opts[:organization], name, version, page)
    else
      Hex.Utils.hexdocs_url(opts[:organization], name, version)
    end
  end

  # TODO: Remove :module support
  defp normalize_module_opt(opts) do
    case Keyword.pop(opts, :module) do
      {nil, opts} ->
        opts

      {module, opts} ->
        Hex.Shell.error("--module is deprecated, use --page instead")
        Keyword.put_new(opts, :page, module)
    end
  end

  defp browser_open(path) do
    Hex.Utils.system_open(path)
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
      |> Enum.sort(&(Version.compare(&1, &2) == :gt))

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
      {:ok, {200, _, body}} ->
        File.mkdir_p!(Path.dirname(target))
        File.write!(target, body)
        true

      {:error, :timeout} ->
        message = """
        Timed out trying to download docs. Try changing your timeout settings:

            HEX_HTTP_TIMEOUT=120 mix hex.docs fetch #{package} #{version}
        """

        Hex.Shell.error(message)

        false

      _ ->
        message = "Couldn't find docs for package with name #{package} or version #{version}"
        Hex.Shell.error(message)
        false
    end
  end

  # Unpacking from a binary limits the decompressed size of the whole tarball,
  # including tar metadata records, which unpacking from a file doesn't count
  defp extract_docs(target, tmp_dir, target_dir) do
    case :mix_hex_tarball.unpack_docs(File.read!(target), String.to_charlist(tmp_dir)) do
      :ok ->
        File.mkdir_p!(Path.dirname(target_dir))
        File.rename!(tmp_dir, target_dir)
        Hex.Shell.info("Docs fetched: #{target_dir}")

      {:error, reason} ->
        Hex.Shell.error("Unpacking docs failed: #{format_unpack_error(reason)}")
    end
  end

  # unpack_docs returns its own errors tagged and tar errors untagged
  defp format_unpack_error({:tarball, {:too_big_uncompressed, size}}),
    do: "docs exceed max uncompressed size of #{div(size, 1024 * 1024)} MiB"

  defp format_unpack_error({:tarball, _} = reason), do: :mix_hex_tarball.format_error(reason)
  defp format_unpack_error(reason), do: :mix_hex_tarball.format_error({:tarball, reason})

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

  defp repo_to_org("hexpm"), do: {:ok, nil}
  defp repo_to_org("hexpm:" <> organization), do: {:ok, organization}
  defp repo_to_org(_repo), do: :error

  # The organization, package name and version are directory names under
  # docs_dir(), so they have to be what hex.pm accepts
  defp validate_package!(organization, name) do
    unless is_nil(organization) or organization_name?(organization) do
      Mix.raise("Invalid organization name: #{inspect(organization)}")
    end

    unless package_name?(name) do
      Mix.raise("Invalid package name: #{inspect(name)}")
    end
  end

  defp validate_version!(version) do
    unless is_binary(version) and match?({:ok, _}, Version.parse(version)) do
      Mix.raise("Invalid version: #{inspect(version)}")
    end
  end

  defp package_name?(<<char, rest::binary>>) when char in ?a..?z, do: name_chars?(rest)
  defp package_name?(_name), do: false

  defp organization_name?(""), do: false
  defp organization_name?(name), do: name_chars?(name)

  defp name_chars?(<<char, rest::binary>>) when char in ?a..?z or char in ?0..?9 or char == ?_,
    do: name_chars?(rest)

  defp name_chars?(<<>>), do: true
  defp name_chars?(_name), do: false

  defp pre_release?(%{"version" => version}), do: do_pre_release?(version)
  defp pre_release?(version), do: do_pre_release?(version)

  defp do_pre_release?(version) do
    case Version.parse(version) do
      {:ok, %Version{pre: []}} -> false
      {:ok, %Version{}} -> true
      _ -> false
    end
  end
end
