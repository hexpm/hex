defmodule Hex.SCM do
  alias Hex.Registry.Server, as: Registry
  @moduledoc false

  @behaviour Mix.SCM
  @packages_dir "packages"
  @request_timeout 60_000
  @fetch_timeout @request_timeout * 2

  def fetchable? do
    true
  end

  def format(_opts) do
    "Hex package"
  end

  def format_lock(opts) do
    case Hex.Utils.lock(opts[:lock]) do
      %{outer_checksum: <<checksum::binary-8, _::binary>>, repo: "hexpm"} = lock ->
        "#{lock.version} (#{lock.name}) #{checksum}"

      %{outer_checksum: <<checksum::binary-8, _::binary>>} = lock ->
        "#{lock.version} (#{lock.repo}/#{lock.name}) #{checksum}"

      %{outer_checksum: nil, repo: "hexpm"} = lock ->
        "#{lock.version} (#{lock.name}) " <>
          ~s("Checksum missing in old lock, run mix deps.get to update")

      %{outer_checksum: nil} = lock ->
        "#{lock.version} (#{lock.repo}/#{lock.name}) " <>
          ~s("Checksum missing in old lock, run mix deps.get to update")

      nil ->
        nil
    end
  end

  def accepts_options(name, opts) do
    opts
    |> organization_to_repo()
    |> Keyword.put_new(:hex, name)
    |> Keyword.put_new(:repo, "hexpm")
    |> Keyword.update!(:hex, &to_string/1)
    |> Keyword.update!(:repo, &to_string/1)
  end

  defp organization_to_repo(opts) do
    case Keyword.fetch(opts, :organization) do
      {:ok, org} ->
        opts
        |> Keyword.delete(:organization)
        |> Keyword.put(:repo, "hexpm:#{org}")

      :error ->
        opts
    end
  end

  def checked_out?(opts) do
    File.dir?(opts[:dest])
  end

  def lock_status(opts) do
    case Hex.Utils.lock(opts[:lock]) do
      %{} = lock ->
        lock_status(
          opts[:dest],
          lock.name,
          lock.version,
          lock.inner_checksum,
          lock.outer_checksum,
          lock.repo
        )

      nil ->
        :mismatch
    end
  end

  defp lock_status(dest, name, version, inner_checksum, outer_checksum, repo) do
    case File.read(Path.join(dest, ".hex")) do
      {:ok, file} ->
        case parse_manifest(file) do
          {:ok, manifest} ->
            match? =
              manifest.name == name and
                manifest.version == version and
                manifest.inner_checksum == inner_checksum and
                (is_nil(manifest.outer_checksum) or is_nil(outer_checksum) or
                   manifest.outer_checksum == outer_checksum) and
                manifest.repo == repo

            if match?, do: :ok, else: :mismatch

          _ ->
            :mismatch
        end

      {:error, _} ->
        :mismatch
    end
  end

  def equal?(opts1, opts2) do
    opts1[:hex] == opts2[:hex] and opts1[:repo] == opts2[:repo]
  end

  def managers(opts) do
    case Hex.Utils.lock(opts[:lock]) do
      %{managers: managers} ->
        managers || []

      nil ->
        []
    end
  end

  def update(opts) do
    Registry.open()

    lock = Hex.Utils.lock(opts[:lock]) |> ensure_lock(opts)
    name = opts[:hex]
    dest = opts[:dest]
    repo = opts[:repo]
    filename = "#{name}-#{lock.version}.tar"
    path = cache_path(repo, filename)
    url = Hex.Repo.get_repo(repo).url <> "/tarballs/#{filename}"

    case Hex.Parallel.await(:hex_fetcher, {:tarball, repo, name, lock.version}, @fetch_timeout) do
      {:ok, :cached} ->
        Hex.Shell.debug("  Using locally cached package (#{path})")

      {:ok, :offline} ->
        Hex.Shell.debug("  [OFFLINE] Using locally cached package (#{path})")

      {:ok, :new, etag} ->
        Registry.tarball_etag(repo, name, lock.version, etag)

        if Version.compare(System.version(), "1.4.0") == :lt do
          Registry.persist()
        end

        safe_url = prune_uri_userinfo(url)
        Hex.Shell.debug("  Fetched package (#{safe_url})")

      {:error, reason} ->
        Hex.Shell.error(reason)

        unless File.exists?(path) do
          safe_url = prune_uri_userinfo(url)
          Mix.raise("Package fetch failed and no cached copy available (#{safe_url})")
        end

        Hex.Shell.info("  Fetch failed. Using locally cached package (#{path})")
    end

    File.rm_rf!(dest)

    registry_inner_checksum =
      Hex.Registry.Server.inner_checksum(repo, to_string(name), lock.version)

    registry_outer_checksum =
      Hex.Registry.Server.outer_checksum(repo, to_string(name), lock.version)

    %{
      inner_checksum: tarball_inner_checksum,
      outer_checksum: tarball_outer_checksum,
      metadata: meta
    } =
      try do
        Hex.unpack_tar!(path, dest)
      rescue
        exception ->
          stacktrace = System.stacktrace()
          File.rm(path)
          reraise(exception, stacktrace)
      end

    if tarball_inner_checksum != registry_inner_checksum do
      File.rm(path)
      raise("Checksum mismatch against registry (inner)")
    end

    if tarball_outer_checksum != registry_outer_checksum do
      File.rm(path)
      raise("Checksum mismatch against registry (outer)")
    end

    build_tools = guess_build_tools(meta)

    managers =
      build_tools
      |> Enum.map(&String.to_atom/1)
      |> Enum.sort()

    manifest =
      encode_manifest(
        name,
        lock.version,
        lock.inner_checksum,
        lock.outer_checksum,
        repo,
        managers
      )

    File.write!(Path.join(dest, ".hex"), manifest)

    deps =
      lock.deps
      |> Enum.map(fn {dep, req, opts} ->
        {dep, req, Keyword.update!(opts, :hex, &String.to_atom/1)}
      end)
      |> Enum.sort()

    {:hex, String.to_atom(lock.name), lock.version, lock.inner_checksum, managers, deps,
     lock.repo, lock.outer_checksum}
  end

  defp prune_uri_userinfo(url) do
    case URI.parse(url) do
      %URI{userinfo: nil} -> url
      uri -> URI.to_string(%{uri | userinfo: "******"})
    end
  end

  def checkout(opts) do
    # For checkout (deps.get) it's important the lock is not modified in the parts of
    # the lock entries that actually locks the dependency. Those entries are the package repo,
    # name, version and checksum. We have additional entries that describe the dependency,
    # the managers and the deps, these are used for optimizations such as avoiding
    # loading the `mix.exs` for dependencies.
    #
    # We need to retrieve the managers in the SCM because the RemoteConverger, where
    # the lock is initially is built does not have this information.
    #
    # Here we update the lock entry with additional information but make sure we only
    # change the lock when the dependency is being updated or changed in some way.

    fetched_lock = update(opts)
    maybe_use_fetched_lock(opts[:lock], fetched_lock)
  end

  defp maybe_use_fetched_lock(current_lock, fetched_lock) do
    case Hex.Utils.lock(current_lock) do
      %{managers: []} -> fetched_lock
      _ -> current_lock
    end
  end

  @build_tools [
    {"mix.exs", "mix"},
    {"rebar.config", "rebar"},
    {"rebar", "rebar"},
    {"Makefile", "make"},
    {"Makefile.win", "make"}
  ]

  def guess_build_tools(%{"build_tools" => tools}) do
    if tools do
      Enum.uniq(tools)
    else
      []
    end
  end

  def guess_build_tools(meta) do
    base_files =
      (meta["files"] || [])
      |> Enum.filter(&(Path.dirname(&1) == "."))
      |> Enum.into(Hex.Set.new())

    Enum.flat_map(@build_tools, fn {file, tool} ->
      if file in base_files do
        [tool]
      else
        []
      end
    end)
    |> Enum.uniq()
  end

  defp ensure_lock(nil, opts) do
    Mix.raise(
      "The lock is missing for package #{opts[:hex]}. This could be " <>
        "because another package has configured the application name " <>
        "for the dependency incorrectly. Verify with the maintainer of " <>
        "the parent application"
    )
  end

  defp ensure_lock(lock, _opts), do: lock

  def parse_manifest(file) do
    case :erlang.binary_to_term(file) do
      {{:hex, 1, _}, map} -> {:ok, add_outer_checksum(map)}
      {{:hex, 2, _}, map} -> {:ok, map}
      _other -> :error
    end
  rescue
    ArgumentError -> {:ok, parse_old_manifest(file)}
  end

  defp add_outer_checksum(%{outer_checksum: _} = map), do: map
  defp add_outer_checksum(map), do: Map.put(map, :outer_checksum, nil)

  defp parse_old_manifest(file) do
    lines =
      file
      |> Hex.string_trim()
      |> String.split("\n")

    case lines do
      [first] ->
        destructure [name, version, inner_checksum, repo], String.split(first, ",")

        %{
          name: name,
          version: version,
          inner_checksum: inner_checksum,
          outer_checksum: nil,
          repo: repo,
          managers: []
        }

      [first, managers] ->
        managers =
          managers
          |> String.split(",")
          |> Enum.map(&String.to_atom/1)

        destructure [name, version, inner_checksum, repo], String.split(first, ",")

        %{
          name: name,
          version: version,
          inner_checksum: inner_checksum,
          outer_checksum: nil,
          repo: repo,
          managers: managers
        }
    end
  end

  defp encode_manifest(name, version, inner_checksum, outer_checksum, repo, managers) do
    map = %{
      name: name,
      version: version,
      inner_checksum: inner_checksum,
      outer_checksum: outer_checksum,
      repo: repo,
      managers: managers || []
    }

    :erlang.term_to_binary({{:hex, 2, 0}, map})
  end

  defp cache_path(repo, name) do
    repo = Hex.Utils.windows_repo_path_fix(repo)
    Path.join([Hex.State.fetch!(:cache_home), @packages_dir, repo, name])
  end

  def prefetch(lock) do
    fetch = fetch_from_lock(lock)

    Enum.each(fetch, fn {repo, package, version} ->
      filename = "#{package}-#{version}.tar"
      path = cache_path(repo, filename)
      etag = if File.exists?(path), do: Registry.tarball_etag(repo, package, version), else: nil

      Hex.Parallel.run(:hex_fetcher, {:tarball, repo, package, version}, fn ->
        fetch(repo, package, version, path, etag)
      end)
    end)
  end

  defp fetch_from_lock(lock) do
    deps_path = Mix.Project.deps_path()

    Enum.flat_map(lock, fn {app, info} ->
      case Hex.Utils.lock(info) do
        %{name: name, version: version, repo: repo} ->
          dest = Path.join(deps_path, "#{app}")

          case lock_status(dest: dest, lock: info) do
            :ok -> []
            :mismatch -> [{repo, name, version}]
            :outdated -> [{repo, name, version}]
          end

        nil ->
          []
      end
    end)
  end

  @doc false
  def fetch(repo, package, version, path, etag) do
    if Hex.State.fetch!(:offline) do
      {:ok, :offline}
    else
      case Hex.Repo.get_tarball(repo, package, version, etag) do
        {:ok, {200, body, headers}} ->
          etag = headers['etag']
          etag = if etag, do: List.to_string(etag)

          case path do
            :memory ->
              {:ok, :new, body, etag}

            _ when is_binary(path) ->
              File.mkdir_p!(Path.dirname(path))
              File.write!(path, body)
              {:ok, :new, etag}
          end

        {:ok, {304, _body, _headers}} ->
          {:ok, :cached}

        {:ok, {code, _body, _headers}} ->
          {:error, "Request failed (#{code})"}

        {:error, :timeout} ->
          reason = [
            "Request failed (:timeout)",
            :reset,
            "\nIf this happens consistently, adjust your concurrency and timeout settings:",
            "\n\n    HEX_HTTP_CONCURRENCY=1 HEX_HTTP_TIMEOUT=120 mix deps.get"
          ]

          {:error, reason}

        {:error, reason} ->
          {:error, "Request failed (#{inspect(reason)})"}
      end
    end
  end
end
