defmodule Hex.OnceCache do
  @moduledoc """
  A cache that computes a value once on first access and caches it.

  Uses Agent.get_and_update/2 to ensure only one process computes the value,
  even when multiple processes access it concurrently. All other processes
  will wait for the Agent call to complete and receive the computed value.

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

  use Agent

  @doc """
  Starts a new OnceCache Agent.

  ## Options

    * `:name` - The name to register the cache under (required)
  """
  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    Agent.start_link(fn -> :not_cached end, name: name)
  end

  @doc """
  Fetches the cached value or computes it if not yet cached.

  The compute function is only called once, even with concurrent access.
  All callers will receive the same computed value.
  """
  def fetch(name, compute_fun) do
    Agent.get_and_update(name, fn
      :not_cached ->
        value = compute_fun.()
        {value, {:cached, value}}

      {:cached, cached} ->
        {cached, {:cached, cached}}
    end)
  end

  @doc """
  Stores a value in the cache without computing it.
  """
  def put(name, value) do
    Agent.update(name, fn _ -> {:cached, value} end)
  end

  @doc """
  Clears the cache.
  """
  def clear(name) do
    Agent.update(name, fn _ -> :not_cached end)
  end
end
