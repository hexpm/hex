defmodule Hex.Policy do
  @moduledoc false

  alias Hex.Registry.Server, as: Registry

  @type ref :: {repo :: String.t(), name :: String.t()}

  @doc """
  Parses a `policy` configuration value into a `{repo, name}` ref.

  Accepts:
    * a keyword list `[repo: "myorg", name: "strict-prod"]` (mix.exs form)
    * a `"repo/name"` string (env-var / `mix hex.config` form)
    * `nil` or `""` (no policy)

  Returns `{:ok, ref}`, `{:ok, nil}`, or `:error`. The bare `"hexpm"` repo is
  rejected because the global hexpm has no organization-scoped policies;
  policies live under `hexpm:<org>` (or any non-`hexpm` repo for self-hosted
  setups).
  """
  @spec parse_config(term()) :: {:ok, ref() | nil} | :error
  def parse_config(nil), do: {:ok, nil}
  def parse_config(""), do: {:ok, nil}
  def parse_config([]), do: {:ok, nil}

  def parse_config(string) when is_binary(string) do
    case String.split(String.trim(string), "/") do
      [repo, name]
      when byte_size(repo) > 0 and byte_size(name) > 0 and repo != "hexpm" ->
        {:ok, {repo, name}}

      _ ->
        :error
    end
  end

  def parse_config([{key, _} | _] = kw) when is_atom(key) do
    case {Keyword.get(kw, :repo), Keyword.get(kw, :name)} do
      {"hexpm", _name} ->
        :error

      {repo, name} when is_binary(repo) and is_binary(name) and repo != "" and name != "" ->
        {:ok, {repo, name}}

      _ ->
        :error
    end
  end

  def parse_config(_), do: :error

  @doc """
  Reads the configured policy ref from `Hex.State`, fetches it through the
  registry, and returns the decoded policy (or `nil` when none is configured).

  A policy is an enforcement feature, so anything short of materializing the
  configured policy fails closed: a malformed configuration value or a fetch
  that yields nothing raises instead of resolving unenforced. Fetch failures
  with no usable cache raise through the registry's standard fetch error path.
  """
  @spec load() :: {:ok, map() | nil}
  def load() do
    case Hex.State.fetch!(:policy) do
      nil ->
        {:ok, nil}

      {:invalid, value} ->
        Mix.raise(
          "Invalid policy configuration: #{inspect(value)}. Expected \"ORG/NAME\" " <>
            "(e.g. \"myorg/strict-prod\") or [repo: \"ORG\", name: \"NAME\"] in mix.exs, " <>
            "where ORG is not the bare \"hexpm\" repo"
        )

      {repo, name} = ref ->
        Registry.open()
        Registry.prefetch_policies([ref])

        case Registry.policy(repo, name) do
          {:ok, decoded} -> {:ok, decoded}
          :error -> Mix.raise("Failed to load policy #{repo}/#{name}")
        end
    end
  end

  @doc """
  Returns the active policy, lazy-loading and caching it in `Hex.State` on
  first call.

  When the remote converger has already populated `:active_policy` (the normal
  `mix deps.get` path) this is a cheap state read. When called standalone (e.g.
  from `mix hex.policy show`) and a policy is configured it triggers the
  registry fetch and stores the result for subsequent calls.
  """
  @spec active() :: {:ok, map() | nil}
  def active() do
    loaded = Hex.State.fetch!(:active_policy)

    cond do
      loaded != nil ->
        {:ok, loaded}

      Hex.State.fetch!(:policy) == nil ->
        {:ok, nil}

      true ->
        {:ok, policy} = load()
        Hex.State.put(:active_policy, policy)
        {:ok, policy}
    end
  end
end
