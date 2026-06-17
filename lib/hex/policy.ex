defmodule Hex.Policy do
  @moduledoc false

  alias Hex.Registry.Server, as: Registry

  @type ref :: {repo :: String.t(), name :: String.t()}

  @doc """
  Validates a `policy` configuration value and normalizes it to the canonical
  `"REPO/NAME"` string.

  Accepts:
    * a keyword list in mix.exs: `[org: "myorg", name: "strict-prod"]` for a
      hexpm organization (resolves to the `hexpm:myorg` repo), or
      `[repo: "REPO", name: "NAME"]` for any configured repo
    * a `"REPO/NAME"` string (env-var / `mix hex.config` form), e.g.
      `"hexpm:myorg/strict-prod"`
    * `nil` or `""` (no policy)

  Returns `{:ok, string}`, `{:ok, nil}`, or `:error`. The bare `"hexpm"` repo
  is rejected because the global hexpm has no organization-scoped policies;
  policies live under `hexpm:<org>` (or any non-`hexpm` repo for self-hosted
  setups). This would be relaxed if hexpm ever serves a global policy (e.g. a
  `recommended` set) directly under the `hexpm` repo.
  """
  @spec parse_config(term()) :: {:ok, String.t() | nil} | :error
  def parse_config(nil), do: {:ok, nil}
  def parse_config(""), do: {:ok, nil}
  def parse_config([]), do: {:ok, nil}

  def parse_config(string) when is_binary(string) do
    string = String.trim(string)

    case parse_ref(string) do
      {:ok, _ref} -> {:ok, string}
      :error -> :error
    end
  end

  def parse_config([{key, _} | _] = kw) when is_atom(key) do
    case {Keyword.get(kw, :repo), Keyword.get(kw, :org), Keyword.get(kw, :name)} do
      {nil, org, name} when is_binary(org) and org != "" and is_binary(name) ->
        parse_config("hexpm:" <> org <> "/" <> name)

      {repo, nil, name} when is_binary(repo) and is_binary(name) ->
        parse_config(repo <> "/" <> name)

      _ ->
        :error
    end
  end

  def parse_config(_), do: :error

  defp parse_ref(string) do
    case String.split(string, "/") do
      [repo, name]
      when byte_size(repo) > 0 and byte_size(name) > 0 and repo != "hexpm" ->
        {:ok, {repo, name}}

      _ ->
        :error
    end
  end

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
          "Invalid policy configuration: #{inspect(value)}. Expected \"REPO/NAME\" " <>
            "(e.g. \"hexpm:myorg/strict-prod\") or [org: \"ORG\", name: \"NAME\"] / " <>
            "[repo: \"REPO\", name: \"NAME\"] in mix.exs, where REPO is not the " <>
            "bare \"hexpm\" repo"
        )

      string when is_binary(string) ->
        {:ok, {repo, name} = ref} = parse_ref(string)
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
