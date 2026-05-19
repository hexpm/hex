defmodule Hex.OnceCache do
  @moduledoc """
  A cache that computes values at most once and caches them.

  Supports both single-value caching via `fetch/3` and keyed caching via
  `fetch_key/4`. Computations run in the caller's process, allowing concurrent
  computations for different keys. Multiple callers requesting the same key
  will wait for the first caller's computation to complete.

  ## Example

      # Start the cache with a name
      {:ok, _} = Hex.OnceCache.start_link(name: MyCache)

      # First call computes and caches
      Hex.OnceCache.fetch(MyCache, fn ->
        IO.puts("Computing...")
        :expensive_result
      end)
      # => :expensive_result

      # Subsequent calls return cached value
      Hex.OnceCache.fetch(MyCache, fn ->
        IO.puts("Computing...")
        :expensive_result
      end)
      # => :expensive_result (no "Computing..." output)
  """

  use GenServer

  @doc """
  Starts a new OnceCache.

  ## Options

    * `:name` - The name to register the cache under (required)
  """
  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  @doc """
  Fetches the cached value or computes it if not yet cached.

  The compute function is only called once, even with concurrent access.
  All callers will receive the same computed value.

  ## Options

    * `:timeout` - The timeout in milliseconds for the fetch operation (default: 5000).
      Use `:infinity` for operations that may take a long time (e.g., user interaction).
  """
  def fetch(name, compute_fun, opts \\ []) do
    fetch_key(name, :__single__, compute_fun, opts)
  end

  @doc """
  Fetches a keyed cached value or computes it if not yet cached.

  Like `fetch/3`, but supports multiple independent cached values identified by key.
  The compute function is only called once per key, even with concurrent access.
  Computations for different keys run concurrently in their respective caller processes.

  Should not be mixed with `fetch/3` or `put/2` on the same cache.
  """
  def fetch_key(name, key, compute_fun, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5000)

    case GenServer.call(name, {:fetch, key}, timeout) do
      {:ok, value} ->
        value

      :compute ->
        try do
          value = compute_fun.()
          :ok = GenServer.call(name, {:computed, key, value}, timeout)
          value
        catch
          kind, reason ->
            GenServer.cast(name, {:failed, key})
            :erlang.raise(kind, reason, __STACKTRACE__)
        end
    end
  end

  @doc """
  Stores a value in the cache without computing it.
  """
  def put(name, value) do
    GenServer.call(name, {:put, :__single__, value})
  end

  @doc """
  Clears the cache.
  """
  def clear(name) do
    GenServer.call(name, :clear)
  end

  # GenServer callbacks

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:fetch, key}, {pid, _} = from, state) do
    case Map.get(state, key) do
      {:cached, value} ->
        {:reply, {:ok, value}, state}

      {:computing, _mon_ref, _waiters} ->
        {:noreply, update_waiters(state, key, from)}

      nil ->
        mon_ref = Process.monitor(pid)
        {:reply, :compute, Map.put(state, key, {:computing, mon_ref, []})}
    end
  end

  def handle_call({:computed, key, value}, _from, state) do
    case Map.get(state, key) do
      {:computing, mon_ref, waiters} ->
        Process.demonitor(mon_ref, [:flush])

        for waiter <- waiters do
          GenServer.reply(waiter, {:ok, value})
        end

        {:reply, :ok, Map.put(state, key, {:cached, value})}

      _ ->
        {:reply, :ok, Map.put(state, key, {:cached, value})}
    end
  end

  def handle_call({:put, key, value}, _from, state) do
    {:reply, :ok, Map.put(state, key, {:cached, value})}
  end

  def handle_call(:clear, _from, _state) do
    {:reply, :ok, %{}}
  end

  @impl true
  def handle_cast({:failed, key}, state) do
    case Map.get(state, key) do
      {:computing, mon_ref, waiters} ->
        Process.demonitor(mon_ref, [:flush])
        {:noreply, hand_off_or_remove(state, key, waiters)}

      _ ->
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:DOWN, mon_ref, :process, _pid, _reason}, state) do
    case find_computing_key(state, mon_ref) do
      {key, waiters} ->
        {:noreply, hand_off_or_remove(state, key, waiters)}

      nil ->
        {:noreply, state}
    end
  end

  defp update_waiters(state, key, from) do
    Map.update!(state, key, fn {:computing, mon_ref, waiters} ->
      {:computing, mon_ref, [from | waiters]}
    end)
  end

  defp hand_off_or_remove(state, key, [{pid, _} = next | rest]) do
    new_mon_ref = Process.monitor(pid)
    GenServer.reply(next, :compute)
    Map.put(state, key, {:computing, new_mon_ref, rest})
  end

  defp hand_off_or_remove(state, key, []) do
    Map.delete(state, key)
  end

  defp find_computing_key(state, mon_ref) do
    Enum.find_value(state, fn
      {key, {:computing, ^mon_ref, waiters}} -> {key, waiters}
      _ -> nil
    end)
  end
end
