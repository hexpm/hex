defmodule Hex.Cooldown do
  @moduledoc false

  @type duration :: String.t()
  @type cutoff :: {:cutoff, integer(), non_neg_integer()} | :disabled

  @doc """
  Parses a duration string into a normalized form for `Hex.State`.

  Returns `{:ok, duration}` for valid input or `:error` for invalid input.

  Accepted forms: `"0"`, `"<N>d"`, `"<N>w"`, `"<N>mo"`.
  """
  @spec parse_config(String.t() | nil) :: {:ok, duration()} | :error
  def parse_config(nil), do: {:ok, "0d"}
  def parse_config(""), do: {:ok, "0d"}

  def parse_config(string) when is_binary(string) do
    case duration_to_seconds(string) do
      {:ok, _} -> {:ok, string}
      :error -> :error
    end
  end

  def parse_config(_), do: :error

  @doc """
  Parses the `cooldown_exclude_repos` config value into a list of repo
  name strings.

  Accepts either a list of strings (project / global config) or a
  comma-separated string (env var). Empty entries are dropped, whitespace
  is trimmed. Repo names should match `Hex.Repo` keys, e.g. `"hexpm"`,
  `"hexpm:myorg"`, or a custom repo name.
  """
  @spec parse_exclude_repos([String.t()] | String.t() | nil) :: {:ok, [String.t()]} | :error
  def parse_exclude_repos(nil), do: {:ok, []}
  def parse_exclude_repos([]), do: {:ok, []}

  def parse_exclude_repos(list) when is_list(list) do
    if Enum.all?(list, &is_binary/1) do
      {:ok, list |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == ""))}
    else
      :error
    end
  end

  def parse_exclude_repos(string) when is_binary(string) do
    {:ok,
     string
     |> String.split(",")
     |> Enum.map(&String.trim/1)
     |> Enum.reject(&(&1 == ""))}
  end

  def parse_exclude_repos(_), do: :error

  @doc """
  Returns true if cooldown should be skipped for the given repo.
  """
  @spec repo_excluded?(String.t() | nil) :: boolean()
  def repo_excluded?(repo) do
    name = repo || "hexpm"
    name in Hex.State.fetch!(:cooldown_exclude_repos)
  end

  @doc """
  Converts a duration string into a number of seconds.
  """
  @spec duration_to_seconds(String.t()) :: {:ok, non_neg_integer()} | :error
  def duration_to_seconds("0"), do: {:ok, 0}

  def duration_to_seconds(string) when is_binary(string) do
    with [_, digits, unit] <- Regex.run(~r/\A(\d+)(d|w|mo)\z/, string),
         {n, ""} <- Integer.parse(digits) do
      {:ok, n * unit_seconds(unit)}
    else
      _ -> :error
    end
  end

  defp unit_seconds("d"), do: 86_400
  defp unit_seconds("w"), do: 86_400 * 7
  defp unit_seconds("mo"), do: 86_400 * 30

  @doc """
  Picks the strictest (longest) duration from a list of `{tag, duration}`
  candidates. `nil`, `""`, and unparseable durations are treated as `"0d"`.

  Returns the chosen `{tag, duration}` so callers can attribute the
  decision to its source (e.g. `:local` vs `{repo, name}`).
  """
  @spec strictest([{tag, String.t() | nil}]) :: {tag, String.t()} when tag: term()
  def strictest(candidates) do
    candidates
    |> Enum.map(fn {tag, dur} ->
      case is_binary(dur) && duration_to_seconds(dur) do
        {:ok, 0} -> {tag, "0d", 0}
        {:ok, seconds} -> {tag, dur, seconds}
        _ -> {tag, "0d", 0}
      end
    end)
    |> Enum.max_by(&elem(&1, 2))
    |> then(fn {tag, dur, _} -> {tag, dur} end)
  end

  @doc """
  Builds a resolution cutoff from the local cooldown configuration.

  Returns `:disabled` when the effective duration is zero.
  """
  @spec build_cutoff() :: cutoff()
  def build_cutoff(), do: build_cutoff(Hex.State.fetch!(:cooldown))

  @doc """
  Builds a resolution cutoff from a duration string.

  `build_cutoff/0` is equivalent to `build_cutoff(Hex.State.fetch!(:cooldown))`.
  """
  @spec build_cutoff(String.t() | nil) :: cutoff()
  def build_cutoff(duration) do
    case duration_to_seconds(duration || "0d") do
      {:ok, 0} ->
        :disabled

      {:ok, seconds} ->
        now = System.system_time(:second)
        {:cutoff, now - seconds, seconds}

      :error ->
        :disabled
    end
  end

  @doc """
  Returns true if the release is eligible under the cutoff.

  A release with no `published_at` (legacy registry data) is treated as
  eligible; cooldown only applies to releases whose publish time is known.
  """
  @spec eligible?(integer() | nil, cutoff()) :: boolean()
  def eligible?(_published_at, :disabled), do: true
  def eligible?(nil, _cutoff), do: true

  def eligible?(published_at, {:cutoff, cutoff_seconds, _}) when is_integer(published_at) do
    published_at <= cutoff_seconds
  end

  @doc """
  Returns the date a release would become eligible under the cutoff.
  """
  @spec eligible_on(integer(), cutoff()) :: Date.t()
  def eligible_on(published_at, {:cutoff, _cutoff_seconds, window_seconds})
      when is_integer(published_at) do
    published_at
    |> Kernel.+(window_seconds)
    |> DateTime.from_unix!()
    |> DateTime.to_date()
  end

  @doc """
  Formats the post-solver summary of versions skipped by cooldown.

  Takes a list of `{repo, package, version, published_at}` tuples accumulated
  during the solve. Returns `nil` when nothing eligible to report — empty
  input, disabled cutoff, or every entry missing a `published_at`.

  Entries are deduplicated and sorted by package then version. Repo is used
  for keying only; it is not rendered.
  """
  @spec format_summary([{String.t(), String.t(), String.t(), integer() | nil}], cutoff()) ::
          String.t() | nil
  def format_summary(_entries, :disabled), do: nil
  def format_summary([], _cutoff), do: nil

  def format_summary(entries, cutoff) do
    entries =
      entries
      |> Enum.reject(fn {_repo, _pkg, _vsn, published_at} -> is_nil(published_at) end)
      |> Enum.uniq()
      |> Enum.sort_by(fn {repo, pkg, vsn, _} -> {repo, pkg, vsn} end)

    case entries do
      [] ->
        nil

      entries ->
        today = Date.utc_today()

        lines =
          Enum.map(entries, fn {_repo, pkg, vsn, published_at} ->
            published_date = published_at |> DateTime.from_unix!() |> DateTime.to_date()
            days_ago = Date.diff(today, published_date)
            eligible_date = eligible_on(published_at, cutoff)
            "  #{pkg} #{vsn} — published #{days_ago} days ago, eligible #{eligible_date}"
          end)

        "\nVersions filtered by cooldown:\n" <> Enum.join(lines, "\n") <> "\n"
    end
  end
end
