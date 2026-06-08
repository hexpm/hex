defmodule Hex.Policy.Sources do
  @moduledoc false

  @type ref :: {repo :: String.t(), name :: String.t()}

  @doc """
  Reads policy refs from all three sources independently and unions them,
  deduplicated. Order: env (`HEX_POLICY`), then project (`mix.exs`),
  then global (`~/.hex/hex.config`). Dedup preserves first-seen order.

  Returns `{:ok, refs}` if every source parses cleanly, otherwise `:error`.
  Each source's parsing is owned by `Hex.State`, which short-circuits to
  its default (`[]`) on a malformed value; this function reports `:error`
  only when a source's `Hex.State` lookup raises.
  """
  @spec load_all() :: {:ok, [ref()]} | :error
  def load_all() do
    refs =
      Hex.State.fetch!(:policy_env) ++
        Hex.State.fetch!(:policy_project) ++
        Hex.State.fetch!(:policy_global)

    {:ok, dedup(refs)}
  rescue
    _ -> :error
  end

  @doc """
  Parses a `policy` configuration value into a list of `{repo, name}` refs.

  Accepts:
    * a single keyword list `[repo: "myorg", name: "strict-prod"]`
    * a list of keyword lists `[[repo: ..., name: ...], ...]`
    * a comma-separated string `"myorg/p,acme/b"` (env-var form)
    * `nil` or `""` (no policies)

  Returns `{:ok, refs}` or `:error`. The bare `"hexpm"` repo is
  rejected because the global hexpm has no organization-scoped
  policies; policies live under `hexpm:<org>` (or any non-`hexpm`
  repo for self-hosted setups).
  """
  @spec parse_config(term()) :: {:ok, [ref()]} | :error
  def parse_config(nil), do: {:ok, []}
  def parse_config(""), do: {:ok, []}
  def parse_config([]), do: {:ok, []}

  def parse_config(string) when is_binary(string) do
    string
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.reduce_while({:ok, []}, fn entry, {:ok, acc} ->
      case String.split(entry, "/") do
        [repo, name]
        when byte_size(repo) > 0 and byte_size(name) > 0 and repo != "hexpm" ->
          {:cont, {:ok, [{repo, name} | acc]}}

        _ ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, refs} -> {:ok, Enum.reverse(refs)}
      :error -> :error
    end
  end

  # Single keyword list — must have BOTH :repo and :name
  def parse_config([{key, _} | _] = kw) when is_atom(key) do
    case to_ref(kw) do
      {:ok, ref} -> {:ok, [ref]}
      :error -> :error
    end
  end

  # List of keyword lists
  def parse_config(list) when is_list(list) do
    Enum.reduce_while(list, {:ok, []}, fn entry, {:ok, acc} ->
      case to_ref(entry) do
        {:ok, ref} -> {:cont, {:ok, [ref | acc]}}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, refs} -> {:ok, Enum.reverse(refs)}
      :error -> :error
    end
  end

  def parse_config(_), do: :error

  defp to_ref(kw) when is_list(kw) do
    case {Keyword.get(kw, :repo), Keyword.get(kw, :name)} do
      {"hexpm", _name} ->
        :error

      {repo, name} when is_binary(repo) and is_binary(name) and repo != "" and name != "" ->
        {:ok, {repo, name}}

      _ ->
        :error
    end
  end

  defp to_ref(_), do: :error

  @doc """
  Deduplicates a list of `{repo, name}` refs, preserving first-seen order.
  """
  @spec dedup([ref()]) :: [ref()]
  def dedup(refs) do
    {result, _seen} =
      Enum.reduce(refs, {[], MapSet.new()}, fn ref, {acc, seen} ->
        if MapSet.member?(seen, ref) do
          {acc, seen}
        else
          {[ref | acc], MapSet.put(seen, ref)}
        end
      end)

    Enum.reverse(result)
  end
end
