defmodule Hex.OnceCacheTest do
  use ExUnit.Case, async: true

  setup do
    cache_name = :"cache_#{:erlang.unique_integer([:positive])}"
    {:ok, _pid} = Hex.OnceCache.start_link(name: cache_name)
    %{cache: cache_name}
  end

  describe "fetch/2" do
    test "computes value on first call", %{cache: cache} do
      pid = self()
      ref = make_ref()

      result =
        Hex.OnceCache.fetch(cache, fn ->
          send(pid, {:computed, ref})
          :computed_value
        end)

      assert result == :computed_value
      assert_received {:computed, ^ref}
    end

    test "returns cached value on subsequent calls", %{cache: cache} do
      pid = self()

      compute_fn = fn ->
        send(pid, :computed)
        :cached_value
      end

      # First call computes
      assert Hex.OnceCache.fetch(cache, compute_fn) == :cached_value
      assert_received :computed

      # Second call uses cached value
      assert Hex.OnceCache.fetch(cache, compute_fn) == :cached_value
      refute_received :computed

      # Third call still uses cached value
      assert Hex.OnceCache.fetch(cache, compute_fn) == :cached_value
      refute_received :computed
    end

    test "handles concurrent calls correctly", %{cache: cache} do
      counter = :counters.new(1, [])

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        # Simulate slow computation
        Process.sleep(50)
        :result
      end

      # Spawn 10 concurrent tasks
      tasks =
        for _ <- 1..10 do
          Task.async(fn ->
            Hex.OnceCache.fetch(cache, compute_fn)
          end)
        end

      # All should return the same result
      results = Task.await_many(tasks)
      assert Enum.all?(results, &(&1 == :result))

      # Compute function should only be called once
      assert :counters.get(counter, 1) == 1
    end

    test "caches nil values", %{cache: cache} do
      counter = :counters.new(1, [])

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        nil
      end

      assert Hex.OnceCache.fetch(cache, compute_fn) == nil
      assert :counters.get(counter, 1) == 1

      # Should use cached nil, not recompute
      assert Hex.OnceCache.fetch(cache, compute_fn) == nil
      assert :counters.get(counter, 1) == 1
    end

    test "caches false values", %{cache: cache} do
      counter = :counters.new(1, [])

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        false
      end

      refute Hex.OnceCache.fetch(cache, compute_fn)
      assert :counters.get(counter, 1) == 1

      # Should use cached false, not recompute
      refute Hex.OnceCache.fetch(cache, compute_fn)
      assert :counters.get(counter, 1) == 1
    end
  end

  describe "put/2" do
    test "stores a value without computing", %{cache: cache} do
      Hex.OnceCache.put(cache, :put_value)

      # fetch should return the put value without calling compute function
      result =
        Hex.OnceCache.fetch(cache, fn ->
          flunk("Compute function should not be called")
        end)

      assert result == :put_value
    end

    test "overwrites previously computed value", %{cache: cache} do
      # First compute a value
      Hex.OnceCache.fetch(cache, fn -> :computed_value end)

      # Then put a different value
      Hex.OnceCache.put(cache, :new_value)

      # fetch should return the new value
      result = Hex.OnceCache.fetch(cache, fn -> :should_not_be_called end)
      assert result == :new_value
    end
  end

  describe "clear/1" do
    test "resets cache to not_fetched state", %{cache: cache} do
      counter = :counters.new(1, [])

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        :computed_value
      end

      # Compute and cache a value
      assert Hex.OnceCache.fetch(cache, compute_fn) == :computed_value
      assert :counters.get(counter, 1) == 1

      # Clear the cache
      Hex.OnceCache.clear(cache)

      # Next fetch should recompute
      assert Hex.OnceCache.fetch(cache, compute_fn) == :computed_value
      assert :counters.get(counter, 1) == 2
    end

    test "clears a put value", %{cache: cache} do
      counter = :counters.new(1, [])

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        :computed_value
      end

      # Put a value
      Hex.OnceCache.put(cache, :put_value)

      # Clear it
      Hex.OnceCache.clear(cache)

      # Next fetch should compute
      assert Hex.OnceCache.fetch(cache, compute_fn) == :computed_value
      assert :counters.get(counter, 1) == 1
    end
  end

  describe "fetch_key/4" do
    test "computes value on first call for a key", %{cache: cache} do
      result =
        Hex.OnceCache.fetch_key(cache, :key1, fn ->
          :value1
        end)

      assert result == :value1
    end

    test "returns cached value on subsequent calls for same key", %{cache: cache} do
      counter = :counters.new(1, [])

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        :value
      end

      assert Hex.OnceCache.fetch_key(cache, :key1, compute_fn) == :value
      assert :counters.get(counter, 1) == 1

      assert Hex.OnceCache.fetch_key(cache, :key1, compute_fn) == :value
      assert :counters.get(counter, 1) == 1
    end

    test "computes independently for different keys", %{cache: cache} do
      assert Hex.OnceCache.fetch_key(cache, :key1, fn -> :value1 end) == :value1
      assert Hex.OnceCache.fetch_key(cache, :key2, fn -> :value2 end) == :value2

      # Both are cached independently
      assert Hex.OnceCache.fetch_key(cache, :key1, fn -> :should_not_compute end) == :value1
      assert Hex.OnceCache.fetch_key(cache, :key2, fn -> :should_not_compute end) == :value2
    end

    test "handles concurrent calls for the same key", %{cache: cache} do
      counter = :counters.new(1, [])

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        Process.sleep(50)
        :result
      end

      tasks =
        for _ <- 1..10 do
          Task.async(fn ->
            Hex.OnceCache.fetch_key(cache, :key1, compute_fn)
          end)
        end

      results = Task.await_many(tasks)
      assert Enum.all?(results, &(&1 == :result))
      assert :counters.get(counter, 1) == 1
    end

    test "computes different keys concurrently", %{cache: cache} do
      # Both keys start computing at the same time.
      # If serialized, total time would be >= 200ms.
      # If concurrent, total time should be ~100ms.
      compute_fn = fn ->
        Process.sleep(100)
        :result
      end

      task1 = Task.async(fn -> Hex.OnceCache.fetch_key(cache, :key1, compute_fn) end)
      task2 = Task.async(fn -> Hex.OnceCache.fetch_key(cache, :key2, compute_fn) end)

      {elapsed, results} = :timer.tc(fn -> Task.await_many([task1, task2]) end)

      assert results == [:result, :result]
      # Should complete in roughly 100ms, not 200ms
      assert elapsed < 180_000
    end

    test "hands off to next waiter when computing process crashes", %{cache: cache} do
      caller = self()

      spawn(fn ->
        Hex.OnceCache.fetch_key(cache, :key1, fn ->
          send(caller, :started)
          Process.sleep(100)
          raise "crash"
        end)
      end)

      assert_receive :started

      task2 =
        Task.async(fn ->
          Hex.OnceCache.fetch_key(cache, :key1, fn -> :recovered end)
        end)

      assert Task.await(task2, 5000) == :recovered
    end

    test "clear resets all keys", %{cache: cache} do
      counter = :counters.new(1, [])

      Hex.OnceCache.fetch_key(cache, :key1, fn -> :value1 end)
      Hex.OnceCache.fetch_key(cache, :key2, fn -> :value2 end)

      Hex.OnceCache.clear(cache)

      compute_fn = fn ->
        :counters.add(counter, 1, 1)
        :recomputed
      end

      assert Hex.OnceCache.fetch_key(cache, :key1, compute_fn) == :recomputed
      assert :counters.get(counter, 1) == 1
    end
  end

  describe "fetch/3 with timeout" do
    test "respects custom timeout for long operations", %{cache: cache} do
      compute_fn = fn ->
        Process.sleep(100)
        :long_operation
      end

      result = Hex.OnceCache.fetch(cache, compute_fn, timeout: 200)
      assert result == :long_operation
    end

    test "waiter times out if computation exceeds timeout", %{cache: cache} do
      # Start a slow computation in another process
      Task.async(fn ->
        Hex.OnceCache.fetch(cache, fn ->
          Process.sleep(200)
          :slow_result
        end)
      end)

      # Give the task time to start computing
      Process.sleep(10)

      # A waiter with a short timeout should time out
      assert catch_exit(Hex.OnceCache.fetch(cache, fn -> :unused end, timeout: 50))
    end

    test "accepts :infinity timeout", %{cache: cache} do
      compute_fn = fn ->
        Process.sleep(100)
        :result
      end

      result = Hex.OnceCache.fetch(cache, compute_fn, timeout: :infinity)
      assert result == :result
    end
  end
end
