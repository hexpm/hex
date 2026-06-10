defmodule Hex.Policy.Filter do
  @moduledoc false

  alias Hex.Cooldown
  alias Hex.Registry.Server

  @severity_order [
    :SEVERITY_NONE,
    :SEVERITY_LOW,
    :SEVERITY_MEDIUM,
    :SEVERITY_HIGH,
    :SEVERITY_CRITICAL
  ]

  @type policy :: map()
  @type candidate :: map()
  @type reason ::
          {:advisory, atom()}
          | {:retirement, atom()}
          | {:cooldown, String.t(), Date.t() | nil}
          | :override_deny

  @doc """
  Classifies a single candidate release against a single policy.

  Finds the `RepositoryPolicy` matching the candidate's repository, evaluates
  its overrides (an `ALLOW` override permits the release and exempts it from
  the restriction; a `DENY` override blocks it; the most specific matching
  override wins), and otherwise applies the restriction limits.

  Returns `:allowed` or `{:blocked, [reason]}`.
  """
  @spec classify(policy(), candidate(), keyword()) :: :allowed | {:blocked, [reason()]}
  def classify(policy, candidate, opts \\ []) do
    case repository_policy(policy, candidate) do
      nil ->
        :allowed

      repo_policy ->
        case override(repo_policy, candidate) do
          :allow -> :allowed
          :deny -> {:blocked, [:override_deny]}
          :none -> restriction(repo_policy, candidate, opts)
        end
    end
  end

  @doc """
  Builds a candidate map for `classify/3` from the registry. Carries the
  metadata the restriction reads: advisories, retirement status, and publish
  time.
  """
  @spec candidate_from_registry(String.t() | nil, String.t(), term()) :: candidate()
  def candidate_from_registry(repo, package, version) do
    version_str = to_string(version)

    %{
      repo: repo || "hexpm",
      package: package,
      version: version_str,
      advisories: Server.advisories(repo, package, version_str) || [],
      retired: Server.retired(repo, package, version_str),
      published_at: Server.published_at(repo, package, version_str)
    }
  end

  # Finds the RepositoryPolicy whose repository matches the candidate's repo.
  # Candidate repos are named "hexpm" or "hexpm:<org>"; the policy's tabs use
  # "hexpm" or the bare org name, so the "hexpm:" prefix is stripped to match.
  defp repository_policy(policy, candidate) do
    repo = candidate_repo(candidate)
    key = strip_prefix(repo)

    Enum.find(Map.get(policy, :repositories, []), fn rp ->
      rp.repository == repo or rp.repository == key
    end)
  end

  defp candidate_repo(%{repo: repo}) when is_binary(repo), do: repo
  defp candidate_repo(_), do: "hexpm"

  defp strip_prefix("hexpm:" <> org), do: org
  defp strip_prefix(repo), do: repo

  # Returns the effect of the most specific override matching the candidate:
  # :allow, :deny, or :none. A requirement-bearing override is more specific
  # than a bare-package one for the same package.
  defp override(repo_policy, candidate) do
    repo_policy
    |> Map.get(:overrides, [])
    |> Enum.filter(&known_action?/1)
    |> Enum.filter(&override_matches?(&1, candidate))
    |> Enum.sort_by(&override_specificity/1, :desc)
    |> case do
      [override | _] -> override_effect(override.action)
      [] -> :none
    end
  end

  # Ignore overrides with an unknown future action (decoded as an integer)
  # rather than crash, mirroring how unknown advisory/retirement enums decode.
  defp known_action?(%{action: action}),
    do: action in [:OVERRIDE_ACTION_ALLOW, :OVERRIDE_ACTION_DENY]

  defp override_matches?(%{ref: %{package: package} = ref}, %{package: package} = candidate) do
    case Map.get(ref, :requirement) do
      nil -> true
      "" -> true
      requirement -> version_satisfies?(candidate.version, requirement)
    end
  end

  defp override_matches?(_override, _candidate), do: false

  defp override_specificity(%{ref: ref}) do
    case Map.get(ref, :requirement) do
      nil -> 0
      "" -> 0
      _ -> 1
    end
  end

  defp override_effect(:OVERRIDE_ACTION_ALLOW), do: :allow
  defp override_effect(:OVERRIDE_ACTION_DENY), do: :deny

  defp restriction(repo_policy, candidate, opts) do
    case Map.get(repo_policy, :restriction) do
      nil ->
        :allowed

      restriction ->
        reasons =
          []
          |> add_advisory(restriction, candidate)
          |> add_retirement(restriction, candidate)
          |> add_cooldown(restriction, candidate, opts)

        if reasons == [], do: :allowed, else: {:blocked, reasons}
    end
  end

  defp add_advisory(reasons, %{advisory_min_severity: threshold}, candidate)
       when not is_nil(threshold) do
    threshold_rank = severity_rank(threshold)
    advisories = Map.get(candidate, :advisories, [])

    if Enum.any?(advisories, fn a -> severity_rank(Map.get(a, :severity)) >= threshold_rank end) do
      [{:advisory, threshold} | reasons]
    else
      reasons
    end
  end

  defp add_advisory(reasons, _restriction, _candidate), do: reasons

  defp add_retirement(reasons, %{retirement_reasons: ret_reasons}, candidate)
       when is_list(ret_reasons) and ret_reasons != [] do
    case Map.get(candidate, :retired) do
      %{reason: retired_atom} ->
        if retired_atom in ret_reasons,
          do: [{:retirement, retired_atom} | reasons],
          else: reasons

      _ ->
        reasons
    end
  end

  defp add_retirement(reasons, _restriction, _candidate), do: reasons

  defp add_cooldown(reasons, %{cooldown: duration}, candidate, opts)
       when is_binary(duration) do
    now = Keyword.get(opts, :now, System.system_time(:second))
    published_at = Map.get(candidate, :published_at)

    case Cooldown.duration_to_seconds(duration) do
      {:ok, seconds} when seconds > 0 and is_integer(published_at) ->
        cutoff = {:cutoff, now - seconds, seconds}

        if Cooldown.eligible?(published_at, cutoff) do
          reasons
        else
          [{:cooldown, duration, Cooldown.eligible_on(published_at, cutoff)} | reasons]
        end

      _ ->
        reasons
    end
  end

  defp add_cooldown(reasons, _restriction, _candidate, _opts), do: reasons

  defp version_satisfies?(version, requirement) do
    with {:ok, version} <- Version.parse(to_string(version)),
         {:ok, requirement} <- Version.parse_requirement(to_string(requirement)) do
      Version.match?(version, requirement)
    else
      _ -> false
    end
  end

  # A severity the client doesn't recognize (a future enum value decoded as an
  # integer, or a missing field) ranks above every known severity so it can
  # never slip under a threshold. Contrast with unknown override actions,
  # which are dropped: ignoring an override falls back to the restriction,
  # while ignoring an advisory would lift it.
  defp severity_rank(severity) do
    Enum.find_index(@severity_order, &(&1 == severity)) || length(@severity_order)
  end
end
