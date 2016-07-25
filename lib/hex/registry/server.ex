defmodule Hex.Registry.Server do
  use GenServer
  @behaviour Hex.Registry

  @name     __MODULE__
  @ets      __MODULE__.ETS
  @filename "index.ets"
  @timeout  60_000

  def start_link do
    GenServer.start_link(__MODULE__, [], name: @name)
  end

  def open(opts) do
    GenServer.call(@name, {:open, opts}, @timeout)
  end

  def close do
    GenServer.call(@name, {:close, [persist: false]}, @timeout)
  end

  def close(name) do
    GenServer.call(name, {:close, []}, @timeout)
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

  def tarball_etag(name, package, version) do
    GenServer.call(name, {:tarball_etag, package, version}, @timeout)
  end

  def tarball_etag(name, package, version, etag) do
    GenServer.call(name, {:tarball_etag, package, version, etag}, @timeout)
  end

  def init([]) do
    {:ok, %{
      ets: nil,
      path: nil,
      refs: %{},
      pending: %{},
      waiting: %{},
      fetched: Hex.Set.new}}
  end

  def handle_call({:open, opts}, _from, %{ets: nil} = state) do
    path = String.to_char_list(opts[:registry_path] || path())
    tid =
      case :ets.file2tab(path) do
        {:ok, tid} -> tid
        {:error, _reason} -> :ets.new(@name, [])
      end
    state = %{state | ets: tid, path: path}
    {:reply, {:ok, self()}, state}
  end
  def handle_call({:open, _opts}, _from, state) do
    {:reply, {:already_open, self()}, state}
  end

  def handle_call({:close, _opts}, _from, %{ets: nil} = state) do
    {:reply, false, state}
  end
  def handle_call({:close, opts}, _from, %{ets: tid, path: path}) do
    if Keyword.get(opts, :persist, true) do
      :ets.tab2file(tid, path)
    end
    :ets.delete(tid)
    {:ok, state} = init([])
    {:reply, true, state}
  end

  def handle_call({:prefetch, packages}, _from, state) do
    packages =
      packages
      |> Enum.uniq
      |> Enum.reject(&(&1 in state.fetched))

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

  def handle_info({ref, result}, state) do
    package = Map.fetch!(state.refs, ref)
    refs = Map.delete(state.refs, ref)
    pending = Map.delete(state.pending, package)
    fetched = Hex.Set.put(state.fetched, package)
    {replys, waiting} = Map.pop(state.waiting, package, [])

    write_result(result, package, state)

    Enum.each(replys, fn {from, fun} ->
      GenServer.reply(from, fun.())
    end)

    state = %{state | refs: refs, pending: pending, waiting: waiting, fetched: fetched}
    {:noreply, state}
  end

  defp prefetch_online(packages, state) do
    tasks =
      Enum.map(packages, fn package ->
        task = Task.async(fn ->
          opts = fetch_opts(package, state)
          Hex.API.Registry.get_package(package, opts)
        end)
        {task.ref, package}
      end)

    refs = Enum.into(tasks, state.refs)
    pending = Enum.into(tasks, state.pending, fn {ref, package} -> {package, ref} end)

    state = %{state | refs: refs, pending: pending}
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

  defp write_result({:http_error, _reason, []}, _package, _state) do
    # TODO
    raise "say what?"
  end

  defp write_result({code, body, headers}, package, %{ets: tid}) when code in 200..299 do
    releases =
      body
      |> :zlib.gunzip
      |> Hex.API.Registry.verify
      |> Hex.API.Registry.decode

    delete_package(package, tid)

    Enum.each(releases, fn %{version: version, checksum: checksum, dependencies: deps} ->
      :ets.insert(tid, {{:checksum, package, version}, checksum})
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

  def maybe_wait(package, from, state, fun) do
    cond do
      package in state.fetched ->
        {:reply, fun.(), state}
      Map.has_key?(state.pending, package) ->
        tuple = {from, fun}
        waiting = Map.update(state.waiting, package, [tuple], &[tuple|&1])
        {:noreply, %{state | waiting: waiting}}
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
    Enum.each(versions, fn version ->
      :ets.delete(tid, {:checksum, package, version})
      :ets.delete(tid, {:deps, package, version})
    end)
  end

  defp lookup(tid, key) do
    case :ets.lookup(tid, key) do
      [{^key, element}] -> element
      [] -> nil
    end
  end
end
