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
  Builds a resolution cutoff from the local cooldown configuration.

  Returns `:disabled` when the effective duration is zero.
  """
  @spec build_cutoff() :: cutoff()
  def build_cutoff() do
    case duration_to_seconds(Hex.State.fetch!(:cooldown)) do
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
  Returns the effective duration string for the configured cutoff.
  """
  @spec describe_duration(cutoff()) :: String.t()
  def describe_duration(:disabled), do: "0"

  def describe_duration({:cutoff, _, window_seconds}) do
    cond do
      rem(window_seconds, 86_400 * 7) == 0 -> "#{div(window_seconds, 86_400 * 7)} weeks"
      true -> "#{div(window_seconds, 86_400)} days"
    end
  end

  @doc """
  Describes the configuration source that contributed the local cooldown.
  Used in error messages.
  """
  @spec describe_source() :: String.t()
  def describe_source() do
    case Hex.State.fetch_source!(:cooldown) do
      {:env, var} -> "#{var}"
      {:project_config, key} -> "mix.exs (#{key})"
      {:global_config, key} -> "~/.hex/hex.config (#{key})"
      :default -> "default"
      :computed -> "runtime"
    end
  end

  @doc """
  Builds the pre-flight error message for a direct dependency whose entire
  matching version set has been filtered by cooldown.
  """
  @spec preflight_error(String.t(), String.t(), [{String.t(), integer()}], cutoff()) :: String.t()
  def preflight_error(package, requirement, filtered, cutoff) do
    today = Date.utc_today()

    lines =
      Enum.map(filtered, fn {version, published_at} ->
        published_date = published_at |> DateTime.from_unix!() |> DateTime.to_date()
        days_ago = Date.diff(today, published_date)
        eligible_date = eligible_on(published_at, cutoff)

        "  #{version} published #{published_date} (#{days_ago} days ago), eligible #{eligible_date}"
      end)

    earliest_eligible =
      filtered
      |> Enum.map(fn {_version, published_at} -> eligible_on(published_at, cutoff) end)
      |> Enum.min(Date)

    duration = describe_duration(cutoff)
    source = describe_source()

    """
    All versions of "#{package}" matching "#{requirement}" are in cooldown:

    #{Enum.join(lines, "\n")}

    Effective cooldown is #{duration} (#{source}).

    To proceed:
      * Wait until #{earliest_eligible} and re-run
      * Bypass for this run: HEX_COOLDOWN=0 mix deps.get
    """
  end

  @doc """
  Note appended to solver failures when cooldown is active and a transitive
  dependency may have been filtered out.
  """
  @spec solver_failure_note(cutoff()) :: String.t() | nil
  def solver_failure_note(:disabled), do: nil

  def solver_failure_note(cutoff) do
    duration = describe_duration(cutoff)

    """

    Note: cooldown is set to #{duration}. If a dependency was filtered because
    it was too recently published, re-run with HEX_COOLDOWN=0 to bypass.
    """
  end
end
