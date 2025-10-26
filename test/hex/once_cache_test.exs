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

  describe "fetch/3 with timeout" do
    test "respects custom timeout for long operations", %{cache: cache} do
      compute_fn = fn ->
        Process.sleep(100)
        :long_operation
      end

      result = Hex.OnceCache.fetch(cache, compute_fn, timeout: 200)
      assert result == :long_operation
    end

    test "times out if computation exceeds timeout", %{cache: cache} do
      compute_fn = fn ->
        Process.sleep(200)
        :long_operation
      end

      assert catch_exit(Hex.OnceCache.fetch(cache, compute_fn, timeout: 50))
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
