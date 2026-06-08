defmodule Hex.Policy do
  @moduledoc false

  alias Hex.Policy.Sources
  alias Hex.Registry.Server, as: Registry

  @doc """
  Reads the configured policy refs from all sources (project, env,
  global), unions them, fetches each policy through the registry,
  and returns the active set as a `%{ref => policy}` map.

  Returns `{:error, :invalid_policy_config}` if any source has a
  malformed value. Fetch failures with no usable cache raise through
  the registry's standard fetch error path.
  """
  @spec load_all() :: {:ok, %{Sources.ref() => map()}} | {:error, term()}
  def load_all() do
    case Sources.load_all() do
      {:ok, []} ->
        {:ok, %{}}

      {:ok, refs} ->
        Registry.open()
        Registry.prefetch_policies(refs)

        policies =
          Enum.reduce(refs, %{}, fn {repo, name} = ref, acc ->
            case Registry.policy(repo, name) do
              {:ok, decoded} -> Map.put(acc, ref, decoded)
              :error -> acc
            end
          end)

        {:ok, policies}

      :error ->
        {:error, :invalid_policy_config}
    end
  end

  @doc """
  Returns the active policy set, lazy-loading and caching it in
  `Hex.State` on first call.

  When the remote converger has already populated `:policies` (the
  normal `mix deps.get` path) this is a cheap state read. When called
  standalone (e.g. from `mix hex.policy show`) and the configured
  source list is non-empty it triggers the registry fetch and stores
  the result for subsequent calls.
  """
  @spec active() :: {:ok, %{Sources.ref() => map()}} | {:error, term()}
  def active() do
    loaded = Hex.State.fetch!(:policies)

    cond do
      loaded != %{} ->
        {:ok, loaded}

      configured_refs() == [] ->
        {:ok, %{}}

      true ->
        case load_all() do
          {:ok, policies_map} ->
            Hex.State.put(:policies, policies_map)
            {:ok, policies_map}

          {:error, _} = err ->
            err
        end
    end
  end

  defp configured_refs() do
    case Sources.load_all() do
      {:ok, refs} -> refs
      :error -> []
    end
  end
end
