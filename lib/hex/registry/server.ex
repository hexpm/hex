defmodule Hex.Registry.Server do
  @moduledoc false

  use GenServer

  @behaviour Hex.Solver.Registry
  @name __MODULE__
  @filename "cache.ets"
  @timeout 60_000
  @ets_version 5
  @public_keys_html "https://hex.pm/docs/public_keys"

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
    case GenServer.call(@name, {:prefetch, packages}, @timeout) do
      :ok -> :ok
      {:error, message} -> Mix.raise(message)
    end
  end

  def prefetch_policies(refs) do
    case GenServer.call(@name, {:prefetch_policies, refs}, @timeout) do
      :ok -> :ok
      {:error, message} -> Mix.raise(message)
    end
  end

  def policy(repo, name) do
    GenServer.call(@name, {:policy, repo, name}, @timeout)
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

  def advisories(repo, package, version) do
    GenServer.call(@name, {:advisories, repo, package, version}, @timeout)
  end

  def published_at(repo, package, version) do
    GenServer.call(@name, {:published_at, repo, package, version}, @timeout)
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
      pending_fun: nil,
      pending_policies: MapSet.new(),
      fetched_policies: MapSet.new(),
      waiting_policies: %{}
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

  def handle_call({:prefetch_policies, refs}, _from, state) do
    refs =
      refs
      |> Enum.map(fn {repo, name} -> {repo || "hexpm", name} end)
      |> Enum.uniq()
      |> Enum.reject(&(&1 in state.fetched_policies))
      |> Enum.reject(&(&1 in state.pending_policies))

    purge_repo_from_cache(refs, state)

    if Hex.State.fetch!(:offline) do
      prefetch_policies_offline(refs, state)
    else
      prefetch_policies_online(refs, state)
    end
  end

  def handle_call({:policy, repo, name}, from, state) do
    maybe_wait_policy({repo, name}, from, state, fn ->
      case lookup(state.ets, {:policy, repo || "hexpm", name}) do
        nil -> :error
        decoded -> {:ok, decoded}
      end
    end)
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

  def handle_call({:advisories, repo, package, version}, from, state) do
    maybe_wait({repo, package}, from, state, fn ->
      lookup(state.ets, {:advisories, repo || "hexpm", package, version})
    end)
  end

  def handle_call({:published_at, repo, package, version}, from, state) do
    maybe_wait({repo, package}, from, state, fn ->
      lookup(state.ets, {:published_at, repo || "hexpm", package, version})
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

  def handle_info({:get_policy, repo, name, result}, state) do
    repo = repo || "hexpm"
    ref = {repo, name}
    pending = MapSet.delete(state.pending_policies, ref)
    fetched = MapSet.put(state.fetched_policies, ref)
    {replys, waiting} = Map.pop(state.waiting_policies, ref, [])

    write_policy_result(result, repo, name, state)

    Enum.each(replys, fn {from, fun} ->
      GenServer.reply(from, fun.())
    end)

    state = %{
      state
      | pending_policies: pending,
        waiting_policies: waiting,
        fetched_policies: fetched
    }

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
  #   {{:advisories, ^repo, _package, _version}, _} -> true
  #   {{:published_at, ^repo, _package, _version}, _} -> true
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
      {{{:advisories, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:published_at, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:registry_etag, :"$1", :"$2"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:timestamp, :"$1", :"$2"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:timestamp, :"$1", :"$2", :"$3"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:policy, :"$1", :"$2"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
      {{{:policy_etag, :"$1", :"$2"}, :_}, [{:"=:=", {:const, repo}, :"$1"}], [true]},
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

  defp prefetch_policies_online(refs, state) do
    Enum.each(refs, fn {repo, name} ->
      etag = policy_etag(repo, name, state)

      Hex.Parallel.run(:hex_fetcher, {:policy, repo, name}, [await: false], fn ->
        {:get_policy, repo, name, Hex.Repo.get_policy(repo, name, etag)}
      end)
    end)

    pending = MapSet.union(MapSet.new(refs), state.pending_policies)
    state = %{state | pending_policies: pending}
    {:reply, :ok, state}
  end

  defp prefetch_policies_offline(refs, state) do
    missing =
      Enum.find(refs, fn {repo, name} ->
        unless lookup(state.ets, {:policy, repo, name}) do
          {repo, name}
        end
      end)

    if missing do
      {repo, name} = missing

      message =
        "Hex is running in offline mode and policy " <>
          "#{repo}/#{name} is not cached locally"

      {:reply, {:error, message}, state}
    else
      fetched = MapSet.union(MapSet.new(refs), state.fetched_policies)
      {:reply, :ok, %{state | fetched_policies: fetched}}
    end
  end

  defp write_result({:ok, {code, headers, %{releases: releases} = result}}, repo, package, %{
         ets: tid
       })
       when code in 200..299 do
    delete_package(repo, package, tid)
    now = :calendar.universal_time()
    pkg_advisories = result[:advisories] || []

    Enum.each(releases, fn %{version: version} = release ->
      :ets.insert(tid, {{:timestamp, repo, package, version}, now})
      :ets.insert(tid, {{:inner_checksum, repo, package, version}, release[:inner_checksum]})
      :ets.insert(tid, {{:outer_checksum, repo, package, version}, release[:outer_checksum]})
      :ets.insert(tid, {{:retired, repo, package, version}, release[:retired]})

      # The registry encodes published_at as a {seconds, nanos} Timestamp
      # map. Cooldown only needs second granularity, so store the integer
      # to keep the consumers simple.
      :ets.insert(
        tid,
        {{:published_at, repo, package, version}, timestamp_seconds(release[:published_at])}
      )

      release_advisories =
        (release[:advisory_indexes] || [])
        |> Enum.map(&Enum.at(pkg_advisories, &1))
        |> Enum.reject(&is_nil/1)

      :ets.insert(tid, {{:advisories, repo, package, version}, release_advisories})

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

  defp write_policy_result({:ok, {code, headers, decoded}}, repo, name, %{ets: tid})
       when code in 200..299 and is_map(decoded) do
    :ets.insert(tid, {{:policy, repo, name}, decoded})

    if etag = headers[~c"etag"] do
      :ets.insert(tid, {{:policy_etag, repo, name}, List.to_string(etag)})
    end
  end

  defp write_policy_result({:ok, {304, _, _}}, _repo, _name, _state) do
    :ok
  end

  defp write_policy_result(other, repo, name, %{ets: tid}) do
    cached? = !!:ets.lookup(tid, {:policy, repo, name})
    print_policy_error(other, repo, name, cached?)

    unless cached? do
      raise "Stopping due to errors"
    end
  end

  defp print_policy_error(result, repo, name, cached?) do
    cached_message = if cached?, do: " (using cache instead)"

    Hex.Shell.error("Failed to fetch policy #{repo}/#{name} from registry#{cached_message}")

    missing? = missing_status?(result)
    unauthorized? = unauthorized_status?(result)

    if missing? or unauthorized? do
      Hex.Shell.error(
        "This could be because the policy does not exist, it was spelled " <>
          "incorrectly or you don't have permissions to it"
      )

      if unauthorized? and not Hex.OAuth.has_tokens?() do
        Hex.Shell.error("No authenticated user found. Run `mix hex.user auth` to authenticate")
      end
    end

    if not (missing? or unauthorized?) or Mix.debug?() do
      Hex.Utils.print_error_result(result)
    end
  end

  defp print_error(result, repo, package, cached?) do
    cached_message = if cached?, do: " (using cache instead)"

    Hex.Shell.error(
      "Failed to fetch record for #{Hex.Utils.package_name(repo, package)} from registry#{cached_message}"
    )

    missing? = missing_status?(result)
    unauthorized? = unauthorized_status?(result)

    if missing? or unauthorized? do
      Hex.Shell.error(
        "This could be because the package does not exist, it was spelled " <>
          "incorrectly or you don't have permissions to it"
      )

      if unauthorized? and not Hex.OAuth.has_tokens?() do
        Hex.Shell.error("No authenticated user found. Run `mix hex.user auth` to authenticate")
      end
    end

    if not (missing? or unauthorized?) or Mix.debug?() do
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

  defp missing_status?({:ok, {status, _, _}}), do: status in [404]
  defp missing_status?(_), do: false

  defp unauthorized_status?({:ok, {status, _, _}}), do: status in [401, 403]
  defp unauthorized_status?(_), do: false

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
        Mix.raise("Package #{repo}/#{package} not prefetched, please report this issue")
    end
  end

  defp maybe_wait_policy({repo, name}, from, state, fun) do
    repo = repo || "hexpm"

    cond do
      {repo, name} in state.fetched_policies ->
        {:reply, fun.(), state}

      {repo, name} in state.pending_policies ->
        tuple = {from, fun}
        waiting = Map.update(state.waiting_policies, {repo, name}, [tuple], &[tuple | &1])
        state = %{state | waiting_policies: waiting}
        {:noreply, state}

      true ->
        Mix.raise("Policy #{repo}/#{name} not prefetched, please report this issue")
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

  defp policy_etag(repo, name, %{ets: tid}) do
    case :ets.lookup(tid, {:policy_etag, repo, name}) do
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
      :ets.delete(tid, {:advisories, repo, package, version})
      :ets.delete(tid, {:published_at, repo, package, version})
      :ets.delete(tid, {:deps, repo, package, version})
    end)
  end

  defp lookup(tid, key) do
    case :ets.lookup(tid, key) do
      [{^key, element}] -> element
      [] -> nil
    end
  end

  defp timestamp_seconds(nil), do: nil
  defp timestamp_seconds(%{seconds: seconds}), do: seconds
end
