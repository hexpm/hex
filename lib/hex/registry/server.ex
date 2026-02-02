defmodule Hex.Registry.Server do
  @moduledoc false

  use GenServer

  @behaviour Hex.Solver.Registry
  @name __MODULE__
  @filename "cache.ets"
  @timeout 60_000
  @ets_version 3
  @public_keys_html "https://hex.pm/docs/public_keys"
  @dashboard_url "https://hex.pm/dashboard"

  def start_link(opts \\ []) do
    opts = Keyword.put_new(opts, :name, @name)
    GenServer.start_link(__MODULE__, [], opts)
  end

  def open(opts \\ []) do
    GenServer.call(@name, {:open, opts}, @timeout)
  end

  def close() do
    GenServer.call(@name, :close, @timeout)
  end

  def persist() do
    GenServer.call(@name, :persist, @timeout)
  end

  def prefetch(packages) do
    :ok = GenServer.call(@name, {:prefetch, packages}, @timeout)
  end

  def versions(repo, package) do
    GenServer.call(@name, {:versions, repo, package}, @timeout)
  end

  def dependencies(repo, package, version) do
    GenServer.call(@name, {:dependencies, repo, package, version}, @timeout)
  end

  def inner_checksum(repo, package, version) do
    GenServer.call(@name, {:inner_checksum, repo, package, version}, @timeout)
  end

  def outer_checksum(repo, package, version) do
    GenServer.call(@name, {:outer_checksum, repo, package, version}, @timeout)
  end

  def retired(repo, package, version) do
    GenServer.call(@name, {:retired, repo, package, version}, @timeout)
  end

  def last_update() do
    GenServer.call(@name, :last_update, @timeout)
  end

  def last_update(time) do
    GenServer.call(@name, {:last_update, time}, @timeout)
  end

  def init([]) do
    {:ok, state()}
  end

  defp state() do
    %{
      ets: nil,
      path: nil,
      pending: MapSet.new(),
      fetched: MapSet.new(),
      waiting: %{},
      pending_fun: nil
    }
  end

  def handle_call({:open, opts}, _from, %{ets: nil} = state) do
    if Keyword.get(opts, :check_version, true) do
      Hex.UpdateChecker.start_check()
    end

    path = opts[:registry_path] || path()

    ets =
      String.to_charlist(path)
      |> open_ets()
      |> check_version()
      |> set_version()

    state = %{state | ets: ets, path: path}

    {:reply, :ok, state}
  end

  def handle_call({:open, opts}, _from, state) do
    if Keyword.get(opts, :check_version, true) do
      Hex.UpdateChecker.start_check()
    end

    {:reply, :ok, state}
  end

  def handle_call(:close, from, state) do
    state =
      wait_pending(state, fn state ->
        if state.ets do
          persist(state.ets, state.path)
          :ets.delete(state.ets)
        end

        GenServer.reply(from, :ok)
        state()
      end)

    {:noreply, state}
  end

  def handle_call(:persist, _from, state) do
    state =
      wait_pending(state, fn state ->
        if state.ets do
          persist(state.ets, state.path)
        end

        state
      end)

    {:reply, :ok, state}
  end

  def handle_call({:prefetch, packages}, _from, state) do
    packages =
      packages
      |> Enum.map(fn {repo, package} -> {repo || "hexpm", package} end)
      |> Enum.uniq()
      |> Enum.reject(&(&1 in state.fetched))
      |> Enum.reject(&(&1 in state.pending))

    purge_repo_from_cache(packages, state)

    if Hex.State.fetch!(:offline) do
      prefetch_offline(packages, state)
    else
      prefetch_online(packages, state)
    end
  end

  def handle_call({:versions, repo, package}, from, state) do
    maybe_wait({repo, package}, from, state, fn ->
      case lookup(state.ets, {:versions, repo || "hexpm", package}) do
        nil ->
          :error

        versions ->
          versions =
            versions
            |> Enum.map(&Hex.Solver.parse_constraint!/1)
            |> Enum.sort(&(Version.compare(&1, &2) in [:lt, :eq]))

          {:ok, versions}
      end
    end)
  end

  def handle_call({:dependencies, repo, package, version}, from, state) do
    maybe_wait({repo, package}, from, state, fn ->
      case lookup(state.ets, {:deps, repo || "hexpm", package, to_string(version)}) do
        nil ->
          :error

        deps ->
          deps =
            Enum.map(deps, fn {repo, package, app, requirement, optional} ->
              %{
                repo: if(repo != "hexpm", do: repo),
                name: package,
                constraint: Hex.Solver.parse_constraint!(requirement || ">= 0.0.0"),
                optional: optional,
                label: app
              }
            end)

          {:ok, deps}
      end
    end)
  end

  def handle_call({:inner_checksum, repo, package, version}, from, state) do
    maybe_wait({repo, package}, from, state, fn ->
      lookup(state.ets, {:inner_checksum, repo || "hexpm", package, version})
    end)
  end

  def handle_call({:outer_checksum, repo, package, version}, from, state) do
    maybe_wait({repo, package}, from, state, fn ->
      lookup(state.ets, {:outer_checksum, repo || "hexpm", package, version})
    end)
  end

  def handle_call({:retired, repo, package, version}, from, state) do
    maybe_wait({repo, package}, from, state, fn ->
      lookup(state.ets, {:retired, repo || "hexpm", package, version})
    end)
  end

  def handle_call(:last_update, _from, state) do
    time = lookup(state.ets, :last_update)
    {:reply, time, state}
  end

  def handle_call({:last_update, time}, _from, state) do
    :ets.insert(state.ets, {:last_update, time})
    {:reply, :ok, state}
  end

  def handle_info({:DOWN, _ref, :process, _pid, :normal}, state) do
    {:noreply, state}
  end

  def handle_info({:get_package, repo, package, result}, state) do
    repo = repo || "hexpm"
    repo_package = {repo, package}
    pending = MapSet.delete(state.pending, repo_package)
    fetched = MapSet.put(state.fetched, repo_package)
    {replys, waiting} = Map.pop(state.waiting, repo_package, [])

    write_result(result, repo, package, state)

    Enum.each(replys, fn {from, fun} ->
      GenServer.reply(from, fun.())
    end)

    state = %{state | pending: pending, waiting: waiting, fetched: fetched}
    state = maybe_run_pending(state)
    {:noreply, state}
  end

  defp open_ets(path) do
    case :ets.file2tab(path, verify: true) do
      {:ok, tid} ->
        tid

      {:error, {:read_error, {:file_error, _path, :enoent}}} ->
        :ets.new(@name, [])

      {:error, reason} ->
        Hex.Shell.error("Error opening ETS file #{path}: #{inspect(reason)}")
        File.rm(path)
        :ets.new(@name, [])
    end
  end

  defp check_version(ets) do
    case :ets.lookup(ets, :version) do
      [{:version, @ets_version}] ->
        ets

      _ ->
        :ets.delete(ets)
        :ets.new(@name, [])
    end
  end

  defp set_version(ets) do
    :ets.insert(ets, {:version, @ets_version})
    ets
  end

  defp persist(tid, path) do
    dir = Path.dirname(path)
    File.mkdir_p!(dir)

    :ok =
      :ets.tab2file(tid, String.to_charlist(path),
        extended_info: [:object_count, :md5sum],
        sync: true
      )
  end

  defp purge_repo_from_cache(packages, %{ets: ets}) do
    Enum.each(packages, fn {repo, _package} ->
      repo = repo || "hexpm"
      config = Hex.Repo.get_repo(repo)
      url = config.url

      case :ets.lookup(ets, {:repo, repo}) do
        [{_key, ^url}] -> :ok
        [] -> :ok
        _ -> purge_repo(repo, ets)
      end

      :ets.insert(ets, {{:repo, repo}, url})
    end)
  end

  # :ets.fun2ms(fn
  #   {{:versions, ^repo, _package}, _} -> true
  #   {{:deps, ^repo, _package, _version}, _} -> true
  #   {{:inner_checksum, ^repo, _package, _version}, _} -> true
  #   {{:outer_checksum, ^repo, _package, _version}, _} -> true
  #   {{:retired, ^repo, _package, _version}, _} -> true
  #   {{:registry_etag, ^repo, _package}, _} -> true
  #   {{:timestamp, ^repo, _package}, _} -> true
  #   {{:timestamp, ^repo, _package, _version}, _} -> true
  #   _ -> false
  # end)

  defp purge_repo_matchspec(repo) do
    [
      {{{:versions, :"$1", :"$2"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:deps, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:inner_checksum, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:outer_checksum, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:retired, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:registry_etag, :"$1", :"$2"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:timetamp, :"$1", :"$2"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:timetamp, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {:_, [], [false]}
    ]
  end

  defp purge_repo(repo, ets) do
    :ets.select_delete(ets, purge_repo_matchspec(repo))
  end

  defp prefetch_online(packages, state) do
    Enum.each(packages, fn {repo, package} ->
      etag = package_etag(repo, package, state)

      Hex.Parallel.run(:hex_fetcher, {:registry, repo, package}, [await: false], fn ->
        {:get_package, repo, package, Hex.Repo.get_package(repo, package, etag)}
      end)
    end)

    pending = MapSet.union(MapSet.new(packages), state.pending)
    state = %{state | pending: pending}
    {:reply, :ok, state}
  end

  defp prefetch_offline(packages, state) do
    missing =
      Enum.find(packages, fn {repo, package} ->
        unless lookup(state.ets, {:versions, repo, package}) do
          package
        end
      end)

    if missing do
      {repo, package} = missing

      message =
        "Hex is running in offline mode and the registry entry for " <>
          "package #{Hex.Utils.package_name(repo, package)} is not cached locally"

      {:reply, {:error, message}, state}
    else
      fetched = MapSet.union(MapSet.new(packages), state.fetched)
      {:reply, :ok, %{state | fetched: fetched}}
    end
  end

  defp write_result({:ok, {code, headers, %{releases: releases}}}, repo, package, %{ets: tid})
       when code in 200..299 do
    delete_package(repo, package, tid)
    now = :calendar.universal_time()

    Enum.each(releases, fn %{version: version} = release ->
      :ets.insert(tid, {{:timestamp, repo, package, version}, now})
      :ets.insert(tid, {{:inner_checksum, repo, package, version}, release[:inner_checksum]})
      :ets.insert(tid, {{:outer_checksum, repo, package, version}, release[:outer_checksum]})
      :ets.insert(tid, {{:retired, repo, package, version}, release[:retired]})

      deps =
        Enum.map(release[:dependencies], fn dep ->
          {dep[:repository] || repo, dep[:package], dep[:app] || dep[:package], dep[:requirement],
           !!dep[:optional]}
        end)

      :ets.insert(tid, {{:deps, repo, package, version}, deps})
    end)

    :ets.insert(tid, {{:timestamp, repo, package}, now})

    versions = Enum.map(releases, & &1[:version])
    :ets.insert(tid, {{:versions, repo, package}, versions})

    if etag = headers[~c"etag"] do
      :ets.insert(tid, {{:registry_etag, repo, package}, List.to_string(etag)})
    end
  end

  defp write_result({:ok, {304, _, _}}, _repo, _package, _state) do
    :ok
  end

  defp write_result(other, repo, package, %{ets: tid}) do
    cached? = !!:ets.lookup(tid, {:versions, repo, package})
    print_error(other, repo, package, cached?)

    unless cached? do
      raise "Stopping due to errors"
    end
  end

  defp print_error(result, repo, package, cached?) do
    cached_message = if cached?, do: " (using cache instead)"

    Hex.Shell.error(
      "Failed to fetch record for #{Hex.Utils.package_name(repo, package)} from registry#{cached_message}"
    )

    if missing_status?(result) do
      print_missing_package_diagnostics(repo, package, result)
    end

    if not missing_status?(result) or Mix.debug?() do
      case result do
        {:error, :bad_signature} ->
          Hex.Shell.error(
            "Could not verify authenticity of fetched registry file because signature verification failed. " <>
              "This may happen because a proxy or some entity is " <>
              "interfering with the download or because you don't have a " <>
              "public key to verify the registry.\n\nYou may try again " <>
              "later or check if a new public key has been released #{public_key_message(repo)}. " <>
              "Set HEX_UNSAFE_REGISTRY=1 to disable this check and allow insecure package downloads."
          )

        {:error, :bad_repo_name} ->
          Hex.Shell.error(
            "The configured repository name for your dependency #{Hex.Utils.package_name(repo, package)} does not " <>
              "match the repository name in the registry. This could be because the repository name is incorrect or " <>
              "because the registry has not been updated to the latest registry format. " <>
              "Set HEX_NO_VERIFY_REPO_ORIGIN=1 to disable this check and allow insecure package downloads."
          )

        _other ->
          Hex.Utils.print_error_result(result)
      end
    end
  end

  @doc false
  def print_missing_package_diagnostics(repo, package, result) do
    {:ok, {status, _headers, _body}} = result
    package_name = Hex.Utils.package_name(repo, package)

    cond do
      # Package does not exist
      status == 404 ->
        Hex.Shell.error(
          "The package #{package_name} does not exist. Please verify the package name is spelled correctly."
        )

      # Permission issue
      status == 403 ->
        auth_status = if has_authentication?(), do: :authenticated, else: :unauthenticated
        print_permission_error(package_name, auth_status)

      true ->
        Hex.Shell.error("Error encounted fetching registry entry for #{package_name}")
    end
  end

  defp print_permission_error(package_name, :authenticated) do
    Hex.Shell.error(
      "You don't have permission to access #{package_name}. This could be because the package is private " <>
        "and you don't have the required permissions. Contact the package owner to request access, or " <>
        "check your permissions at: #{@dashboard_url}"
    )
  end

  defp print_permission_error(package_name, :unauthenticated) do
    Hex.Shell.error(
      "You don't have permission to access #{package_name}. This could be because the package is private " <>
        "and requires authentication, run 'mix hex.user auth' to authenticate."
    )
  end

  defp has_authentication? do
    # Check if user has OAuth token
    has_oauth = Hex.OAuth.has_tokens?()

    # Check for API keys
    repos_key = Hex.State.get(:repos_key)
    api_key = Hex.State.get(:api_key)

    has_oauth || repos_key != nil || api_key != nil
  end

  defp missing_status?({:ok, {status, _, _}}), do: status in [403, 404]
  defp missing_status?(_), do: false

  defp public_key_message("hexpm:" <> _), do: "on our public keys page: #{@public_keys_html}"
  defp public_key_message("hexpm"), do: "on our public keys page: #{@public_keys_html}"
  defp public_key_message(repo), do: "for repo #{repo}"

  defp maybe_wait({repo, package}, from, state, fun) do
    repo = repo || "hexpm"

    cond do
      {repo, package} in state.fetched ->
        {:reply, fun.(), state}

      {repo, package} in state.pending ->
        tuple = {from, fun}
        waiting = Map.update(state.waiting, {repo, package}, [tuple], &[tuple | &1])
        state = %{state | waiting: waiting}
        {:noreply, state}

      true ->
        repo = if repo, do: "#{repo}/"
        Mix.raise("Package #{repo}#{package} not prefetched, please report this issue")
    end
  end

  defp wait_pending(state, fun) do
    if MapSet.size(state.pending) == 0 do
      state = fun.(state)
      %{state | pending_fun: nil}
    else
      %{state | pending_fun: fun}
    end
  end

  defp maybe_run_pending(%{pending_fun: nil} = state) do
    state
  end

  defp maybe_run_pending(%{pending_fun: fun} = state) do
    wait_pending(state, fun)
  end

  defp package_etag(repo, package, %{ets: tid}) do
    case :ets.lookup(tid, {:registry_etag, repo, package}) do
      [{_, etag}] -> etag
      [] -> nil
    end
  end

  defp path do
    Path.join(Hex.State.fetch!(:cache_home), @filename)
  end

  defp delete_package(repo, package, tid) do
    :ets.delete(tid, {:registry_etag, repo, package})
    versions = lookup(tid, {:versions, repo, package}) || []
    :ets.delete(tid, {:versions, repo, package})

    Enum.each(versions, fn version ->
      :ets.delete(tid, {:checksum, repo, package, version})
      :ets.delete(tid, {:retired, repo, package, version})
      :ets.delete(tid, {:deps, repo, package, version})
    end)
  end

  defp lookup(tid, key) do
    case :ets.lookup(tid, key) do
      [{^key, element}] -> element
      [] -> nil
    end
  end
end
