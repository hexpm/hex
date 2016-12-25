defmodule Hex.Registry.Server do
  use GenServer
  @behaviour Hex.Registry

  # TODO: Optimize to not go through genserver

  @name __MODULE__
  @filename "cache.ets"
  @timeout 60_000
  @update_interval 24 * 60 * 60

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, @name)
    opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, [], opts)
  end

  def open(name \\ @name, opts) do
    GenServer.call(name, {:open, opts}, @timeout)
  end

  def close do
    GenServer.call(@name, :close, @timeout)
    |> print_update_message
  end

  def close(name) do
    GenServer.call(name, :close, @timeout)
    |> print_update_message
  end

  def persist do
    GenServer.call(@name, :persist, @timeout)
    |> print_update_message
  end

  def check_update do
    GenServer.cast(@name, :check_update)
  end

  def prefetch(name, packages) do
    case GenServer.call(name, {:prefetch, packages}, @timeout) do
      :ok ->
        :ok
      {:error, package} ->
        Mix.raise "Hex is running in offline mode and the registry entry for " <>
                  "package #{package} is not cached locally"
    end
  end

  def versions(name, package) do
    GenServer.call(name, {:versions, package}, @timeout)
  end

  def deps(name, package, version) do
    GenServer.call(name, {:deps, package, version}, @timeout)
  end

  def checksum(name, package, version) do
    GenServer.call(name, {:checksum, package, version}, @timeout)
  end

  def retired(name, package, version) do
    GenServer.call(name, {:retired, package, version}, @timeout)
  end

  def tarball_etag(name, package, version) do
    GenServer.call(name, {:tarball_etag, package, version}, @timeout)
  end

  def tarball_etag(name, package, version, etag) do
    GenServer.call(name, {:tarball_etag, package, version, etag}, @timeout)
  end

  defp print_update_message({:update, {:http_error, reason}}) do
    Hex.Shell.error "Hex update check failed, HTTP ERROR: #{inspect reason}"
    :ok
  end
  defp print_update_message({:update, {:status, status}}) do
    Hex.Shell.error "Hex update check failed, status code: #{status}"
    :ok
  end
  defp print_update_message({:update, version}) do
    Hex.Shell.warn "A new Hex version is available (#{Hex.version} < #{version}), " <>
                   "please update with `mix local.hex`"
    :ok
  end
  defp print_update_message(:ok), do: :ok

  def init([]) do
    {:ok, reset_state(%{})}
  end

  defp reset_state(state) do
    %{ets: nil,
      path: nil,
      pending: Hex.Set.new,
      fetched: Hex.Set.new,
      waiting: %{},
      waiting_close: nil,
      already_checked_update?: Map.get(state, :already_checked_update?, false),
      checking_update?: false,
      new_update: nil}
  end

  def handle_cast(:check_update, state) do
    state = check_update(state, force: true)
    {:noreply, state}
  end

  def handle_call({:open, opts}, _from, %{ets: nil} = state) do
    path = Hex.string_to_charlist(opts[:registry_path] || path())
    tid = open_ets(path)
    state = %{state | ets: tid, path: path}
    state = check_update(state, force: false)
    {:reply, {:ok, self()}, state}
  end
  def handle_call({:open, _opts}, _from, state) do
    {:reply, {:already_open, self()}, state}
  end

  def handle_call(:close, from, state) do
    maybe_wait_closing(state, from, fn
      %{ets: nil} = state ->
        state
      %{ets: tid, path: path} ->
        persist(tid, path)
        :ets.delete(tid)
        reset_state(state)
    end)
  end

  def handle_call(:persist, from, state) do
    maybe_wait_closing(state, from, fn %{ets: tid, path: path} = state ->
      persist(tid, path)
      state
    end)
  end

  def handle_call({:prefetch, packages}, _from, state) do
    packages =
      packages
      |> Enum.uniq
      |> Enum.reject(&(&1 in state.fetched))
      |> Enum.reject(&(&1 in state.pending))

    if Hex.State.fetch!(:offline?) do
      prefetch_offline(packages, state)
    else
      prefetch_online(packages, state)
    end
  end

  def handle_call({:versions, package}, from, state) do
    maybe_wait(package, from, state, fn ->
      lookup(state.ets, {:versions, package})
    end)
  end

  def handle_call({:deps, package, version}, from, state) do
    maybe_wait(package, from, state, fn ->
      lookup(state.ets, {:deps, package, version})
    end)
  end

  def handle_call({:checksum, package, version}, from, state) do
    maybe_wait(package, from, state, fn ->
      lookup(state.ets, {:checksum, package, version})
    end)
  end

  def handle_call({:retired, package, version}, from, state) do
    maybe_wait(package, from, state, fn ->
      lookup(state.ets, {:retired, package, version})
    end)
  end

  def handle_call({:tarball_etag, package, version}, _from, state) do
    etag = lookup(state.ets, {:tarball_etag, package, version})
    {:reply, etag, state}
  end

  def handle_call({:tarball_etag, package, version, etag}, _from, state) do
    :ets.insert(state.ets, {{:tarball_etag, package, version}, etag})
    {:reply, :ok, state}
  end

  def handle_info({:DOWN, _ref, :process, _pid, :normal}, state) do
    {:noreply, state}
  end

  def handle_info({_ref, {:get_installs, result}}, state) do
    result =
      case result do
        {code, body, _headers} when code in 200..299 ->
          Hex.API.Registry.find_new_version_from_csv(body)
        {code, body, _} ->
          Hex.Shell.error("Failed to check for new Hex version")
          Hex.Utils.print_error_result(code, body)
          nil
      end

    :ets.insert(state.ets, {:last_update, :calendar.universal_time})
    state = reply_to_update_waiting(state, result)
    state = %{state | checking_update?: false}
    {:noreply, state}
  end

  def handle_info({:get_package, package, result}, state) do
    pending = Hex.Set.delete(state.pending, package)
    fetched = Hex.Set.put(state.fetched, package)
    {replys, waiting} = Map.pop(state.waiting, package, [])

    write_result(result, package, state)

    Enum.each(replys, fn {from, fun} ->
      GenServer.reply(from, fun.())
    end)

    state = %{state | pending: pending, waiting: waiting, fetched: fetched}
    {:noreply, state}
  end

  defp open_ets(path) do
    case :ets.file2tab(path) do
      {:ok, tid} ->
        tid
      {:error, {:read_error, {:file_error, _path, :enoent}}} ->
        :ets.new(@name, [])
      {:error, reason} ->
        Hex.Shell.error("Error opening ETS file #{path}: #{inspect reason}")
        File.rm(path)
        :ets.new(@name, [])
    end
  end

  defp persist(tid, path) do
    dir = Path.dirname(path)
    File.mkdir_p!(dir)
    :ok = :ets.tab2file(tid, path)
  end

  defp prefetch_online(packages, state) do
    Enum.each(packages, fn package ->
      opts = fetch_opts(package, state)
      Hex.Parallel.run(:hex_fetcher, {:registry, package}, [await: false], fn ->
        {:get_package, package, Hex.API.Registry.get_package(package, opts)}
      end)
    end)

    pending = Enum.into(packages, state.pending)
    state = %{state | pending: pending}
    {:reply, :ok, state}
  end

  defp prefetch_offline(packages, state) do
    missing =
      Enum.find(packages, fn package ->
        unless lookup(state.ets, {:versions, package}), do: package
      end)

    if missing do
      {:reply, {:error, missing}, state}
    else
      fetched = Enum.into(packages, state.fetched)
      {:reply, :ok, %{state | fetched: fetched}}
    end
  end

  defp write_result({code, body, headers}, package, %{ets: tid}) when code in 200..299 do
    releases =
      body
      |> :zlib.gunzip
      |> Hex.API.Registry.verify
      |> Hex.API.Registry.decode

    delete_package(package, tid)

    Enum.each(releases, fn %{version: version, checksum: checksum, dependencies: deps} = release ->
      :ets.insert(tid, {{:checksum, package, version}, checksum})
      :ets.insert(tid, {{:retired, package, version}, release[:retired]})
      deps = Enum.map(deps, fn dep ->
        {dep[:package], dep[:app] || dep[:package], dep[:requirement], !!dep[:optional]}
      end)
      :ets.insert(tid, {{:deps, package, version}, deps})
    end)

    versions = Enum.map(releases, & &1[:version])
    :ets.insert(tid, {{:versions, package}, versions})

    if etag = headers['etag'] do
      :ets.insert(tid, {{:registry_etag, package}, List.to_string(etag)})
    end
  end
  defp write_result({304, _, _}, _package, _state) do
    :ok
  end
  defp write_result({404, _, _}, package, %{ets: tid}) do
    delete_package(package, tid)
    :ok
  end

  defp write_result({code, body, _}, package, %{ets: tid}) do
    cached? = !!:ets.lookup(tid, {:versions, package})
    cached_message = if cached?, do: " (using cache)"
    Hex.Shell.error("Failed to fetch record for '#{package}' from registry#{cached_message}")
    Hex.Utils.print_error_result(code, body)

    unless cached? do
      raise "Stopping due to errors"
    end
  end

  defp maybe_wait(package, from, state, fun) do
    cond do
      package in state.fetched ->
        {:reply, fun.(), state}
      package in state.pending ->
        tuple = {from, fun}
        waiting = Map.update(state.waiting, package, [tuple], &[tuple|&1])
        state = %{state | waiting: waiting}
        {:noreply, state}
      true ->
        raise "Package #{package} not prefetched, please report this issue"
    end
  end

  defp fetch_opts(package, %{ets: tid}) do
    case :ets.lookup(tid, {:registry_etag, package}) do
      [{_, etag}] -> [etag: etag]
      [] -> []
    end
  end

  defp path do
    Path.join(Hex.State.fetch!(:home), @filename)
  end

  defp delete_package(package, tid) do
    :ets.delete(tid, {:registry_etag, package})
    versions = lookup(tid, {:versions, package}) || []
    :ets.delete(tid, {:versions, package})
    Enum.each(versions, fn version ->
      :ets.delete(tid, {:checksum, package, version})
      :ets.delete(tid, {:retired, package, version})
      :ets.delete(tid, {:deps, package, version})
    end)
  end

  defp lookup(tid, key) do
    case :ets.lookup(tid, key) do
      [{^key, element}] -> element
      [] -> nil
    end
  end

  def maybe_wait_closing(%{checking_update?: true, new_update: nil} = state, from, fun) do
    state = %{state | waiting_close: {from, fun}}
    {:noreply, state}
  end
  def maybe_wait_closing(%{checking_update?: false, new_update: nil} = state, _from, fun) do
    {:reply, :ok, fun.(state)}
  end
  def maybe_wait_closing(%{checking_update?: false, new_update: new_update} = state, _from, fun) do
    state = %{state | new_update: nil}
    {:reply, {:update, new_update}, fun.(state)}
  end

  defp reply_to_update_waiting(state, new_update) do
    case state.waiting_close do
      {from, fun} ->
        reply = if new_update, do: {:update, new_update}, else: :ok
        state = fun.(state)
        GenServer.reply(from, reply)
        %{state | waiting_close: nil}
      nil ->
        %{state | new_update: new_update}
    end
  end

  defp check_update(%{already_checked_update?: true} = state, _opts) do
    state
  end
  defp check_update(%{checking_update?: true} = state, _opts) do
    state
  end
  defp check_update(%{ets: tid} = state, opts) do
    if opts[:force] || check_update?(tid) do
      Task.async(fn ->
        {:get_installs, Hex.API.Registry.get_installs}
      end)

      %{state | checking_update?: true, already_checked_update?: true}
    else
      state
    end
  end

  defp check_update?(tid) do
    if last = lookup(tid, :last_update) do
      now = :calendar.universal_time |> :calendar.datetime_to_gregorian_seconds
      last = :calendar.datetime_to_gregorian_seconds(last)

      now - last > @update_interval
    else
      true
    end
  end
end
