defmodule Hex.Registry do
  @name __MODULE__

  def start_link do
    Agent.start_link(fn -> [] end, name: @name)
  end

  def append({key, registry, scm}) do
    case Hex.PackageRegistrySupervisor.start_registry(registry) do
      {:ok, _pid} ->
        Agent.update(@name, &(&1 ++ [{key, registry, scm, nil}]))
        Hex.PackageRegistry.append(registry)
      {:error, reason} -> {:error, reason}
    end
  end

  def keys do
    Agent.get(@name, & &1)
    |> Enum.map(&elem(&1, 0))
  end

  def has_scm?(scm), do: !is_nil(by_scm(scm))

  def by_scm(scm) do
    Agent.get(@name, & &1)
    |> Enum.find(fn
      {_, _, ^scm, _} -> true
      _ -> false
    end)
  end

  def prefetch(lock) do
    @name
    |> Agent.get(& &1)
    |> Enum.each(fn {_, _, scm, _} -> scm.prefetch(lock) end)
  end

  def verify_lock_tuple(lock_tuple, default \\ nil, callback)
  def verify_lock_tuple({key, name, version}, default, callback) do
    if key in keys do
      callback.({key, name, version})
    else
      default
    end
  end
  def verify_lock_tuple(_lock_tuple, default, _callback), do: default
end
