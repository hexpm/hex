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

  @retirement_reasons [
    :RETIRED_OTHER,
    :RETIRED_INVALID,
    :RETIRED_SECURITY,
    :RETIRED_DEPRECATED,
    :RETIRED_RENAMED
  ]

  @override_actions [
    :OVERRIDE_ACTION_ALLOW,
    :OVERRIDE_ACTION_DENY,
    :OVERRIDE_ACTION_ADVISORY,
    :OVERRIDE_ACTION_RETIREMENT,
    :OVERRIDE_ACTION_COOLDOWN
  ]

  @policy_string_disallowed_ranges [
    {0x0000, 0x001F},
    {0x007F, 0x009F},
    {0x00AD, 0x00AD},
    {0x0600, 0x0605},
    {0x061C, 0x061C},
    {0x06DD, 0x06DD},
    {0x070F, 0x070F},
    {0x0890, 0x0891},
    {0x08E2, 0x08E2},
    {0x180E, 0x180E},
    {0x200B, 0x200F},
    {0x2028, 0x202E},
    {0x2060, 0x2064},
    {0x2066, 0x206F},
    {0xFEFF, 0xFEFF},
    {0xFFF9, 0xFFFB},
    {0x110BD, 0x110BD},
    {0x110CD, 0x110CD},
    {0x13430, 0x1343F},
    {0x1BCA0, 0x1BCA3},
    {0x1D173, 0x1D17A},
    {0xE0001, 0xE0001},
    {0xE0020, 0xE007F}
  ]

  @type policy :: map()
  @type candidate :: map()
  @type reason ::
          {:advisory, atom()}
          | {:retirement, atom()}
          | {:cooldown, String.t(), Date.t() | nil}
          | :override_deny
  @type acceptance :: %{
          source: :override | :policy,
          kind: :allow | :deny | :advisory | :retirement | :cooldown | :policy,
          identifier: String.t() | atom() | nil,
          comment: String.t() | nil
        }
  @type finding :: {:advisory, map()} | {:retirement, map()}

  @doc """
  Classifies a single candidate release against a single policy.

  Finds the `RepositoryPolicy` matching the candidate's repository, evaluates
  its final overrides (an `ALLOW` override permits the release and exempts it
  from the restriction; a `DENY` override blocks it; the most specific
  matching override wins), and otherwise applies scoped overrides and the
  restriction limits.

  Returns `:allowed` or `{:blocked, [reason]}`.
  """
  @spec classify(policy(), candidate(), keyword()) :: :allowed | {:blocked, [reason()]}
  def classify(policy, candidate, opts \\ []) do
    case explain(policy, candidate, opts) do
      {:allowed, _acceptances} -> :allowed
      {:blocked, reasons, _acceptances} -> {:blocked, reasons}
    end
  end

  @doc """
  Classifies a release and includes policy overrides that affected the decision.
  """
  @spec explain(policy(), candidate(), keyword()) ::
          {:allowed, [acceptance()]} | {:blocked, [reason()], [acceptance()]}
  def explain(policy, candidate, opts \\ []) do
    case repository_policy(policy, candidate) do
      nil ->
        {:allowed, []}

      repo_policy ->
        case final_override(repo_policy, candidate) do
          {:allow, override} ->
            {:allowed, [override_acceptance(override)]}

          {:deny, override} ->
            {:blocked, [:override_deny], [override_acceptance(override)]}

          :none ->
            acceptances = scoped_override_acceptances(repo_policy, candidate, opts)

            case restriction(repo_policy, candidate, opts) do
              :allowed -> {:allowed, acceptances}
              {:blocked, reasons} -> {:blocked, reasons, acceptances}
            end
        end
    end
  end

  @doc """
  Evaluates one advisory or retirement finding for a policy-aware audit.

  `:overrides` applies matching ALLOW, ADVISORY, and RETIREMENT overrides.
  `:policy` also applies the policy's advisory and retirement restrictions.
  """
  @spec audit_finding(policy(), candidate(), finding(), :overrides | :policy) ::
          :active | {:accepted, acceptance()}
  def audit_finding(policy, candidate, finding, mode) when mode in [:overrides, :policy] do
    case repository_policy(policy, candidate) do
      nil ->
        if mode == :policy, do: {:accepted, acceptance(:policy)}, else: :active

      repo_policy ->
        case final_override(repo_policy, candidate) do
          {:allow, override} ->
            {:accepted, override_acceptance(override)}

          {:deny, _override} ->
            :active

          :none ->
            case matching_scoped_override(repo_policy, candidate, finding) do
              nil -> audit_restriction(repo_policy, finding, mode)
              override -> {:accepted, override_acceptance(override)}
            end
        end
    end
  end

  @doc """
  Returns the explanation surfaced for a policy-accepted audit finding.
  """
  @spec acceptance_message(acceptance()) :: String.t()
  def acceptance_message(%{comment: comment}) when is_binary(comment) and comment != "",
    do: comment

  def acceptance_message(_acceptance), do: "Accepted by the active dependency policy."

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

  # A requirement-bearing final override is more specific than a bare-package one.
  defp final_override(repo_policy, candidate) do
    repo_policy
    |> Map.get(:overrides, [])
    |> matching_final_override(candidate)
    |> final_override_effect()
  end

  defp matching_final_override(overrides, candidate) do
    overrides
    |> Enum.filter(&valid_override?/1)
    |> Enum.filter(&(Map.get(&1, :action) in [:OVERRIDE_ACTION_ALLOW, :OVERRIDE_ACTION_DENY]))
    |> Enum.filter(&ref_matches?(&1.ref, candidate))
    |> Enum.sort_by(&override_specificity/1, :desc)
    |> List.first()
  end

  defp final_override_effect(%{action: :OVERRIDE_ACTION_ALLOW} = override),
    do: {:allow, override}

  defp final_override_effect(%{action: :OVERRIDE_ACTION_DENY} = override),
    do: {:deny, override}

  defp final_override_effect(nil), do: :none

  defp restriction(repo_policy, candidate, opts) do
    case Map.get(repo_policy, :restriction) do
      nil ->
        :allowed

      restriction ->
        reasons =
          []
          |> add_advisory(repo_policy, restriction, candidate)
          |> add_retirement(repo_policy, restriction, candidate)
          |> add_cooldown(repo_policy, restriction, candidate, opts)

        if reasons == [], do: :allowed, else: {:blocked, reasons}
    end
  end

  defp add_advisory(reasons, repo_policy, %{advisory_min_severity: threshold}, candidate)
       when not is_nil(threshold) do
    threshold_rank = severity_rank(threshold)

    advisories =
      candidate
      |> Map.get(:advisories, [])
      |> Enum.reject(fn advisory ->
        matching_scoped_override(repo_policy, candidate, {:advisory, advisory}) != nil
      end)

    if Enum.any?(advisories, fn a -> severity_rank(Map.get(a, :severity)) >= threshold_rank end) do
      [{:advisory, threshold} | reasons]
    else
      reasons
    end
  end

  defp add_advisory(reasons, _repo_policy, _restriction, _candidate), do: reasons

  defp add_retirement(reasons, repo_policy, %{retirement_reasons: ret_reasons}, candidate)
       when is_list(ret_reasons) and ret_reasons != [] do
    case Map.get(candidate, :retired) do
      %{reason: retired_atom} = retired ->
        override = matching_scoped_override(repo_policy, candidate, {:retirement, retired})

        if retired_atom in ret_reasons and is_nil(override),
          do: [{:retirement, retired_atom} | reasons],
          else: reasons

      _ ->
        reasons
    end
  end

  defp add_retirement(reasons, _repo_policy, _restriction, _candidate), do: reasons

  defp add_cooldown(reasons, repo_policy, %{cooldown: duration}, candidate, opts)
       when is_binary(duration) do
    now = Keyword.get(opts, :now, System.system_time(:second))
    published_at = Map.get(candidate, :published_at)

    case Cooldown.duration_to_seconds(duration) do
      {:ok, seconds} when seconds > 0 and is_integer(published_at) ->
        cutoff = {:cutoff, now - seconds, seconds}

        if Cooldown.eligible?(published_at, cutoff) or
             matching_scoped_override(repo_policy, candidate, :cooldown) != nil do
          reasons
        else
          [{:cooldown, duration, Cooldown.eligible_on(published_at, cutoff)} | reasons]
        end

      _ ->
        reasons
    end
  end

  defp add_cooldown(reasons, _repo_policy, _restriction, _candidate, _opts), do: reasons

  defp audit_restriction(_repo_policy, _finding, :overrides), do: :active

  defp audit_restriction(repo_policy, {:advisory, advisory}, :policy) do
    case Map.get(repo_policy, :restriction) do
      %{advisory_min_severity: threshold} when not is_nil(threshold) ->
        if severity_rank(Map.get(advisory, :severity)) >= severity_rank(threshold),
          do: :active,
          else: {:accepted, acceptance(:policy)}

      _restriction ->
        {:accepted, acceptance(:policy)}
    end
  end

  defp audit_restriction(repo_policy, {:retirement, retired}, :policy) do
    reason = Map.get(retired, :reason)

    case Map.get(repo_policy, :restriction) do
      %{retirement_reasons: reasons} when is_list(reasons) and reasons != [] ->
        if reason in reasons, do: :active, else: {:accepted, acceptance(:policy)}

      _restriction ->
        {:accepted, acceptance(:policy)}
    end
  end

  defp scoped_override_acceptances(repo_policy, candidate, opts) do
    advisory_acceptances =
      candidate
      |> Map.get(:advisories, [])
      |> Enum.flat_map(fn advisory ->
        case matching_scoped_override(repo_policy, candidate, {:advisory, advisory}) do
          nil -> []
          override -> [override_acceptance(override)]
        end
      end)

    retirement_acceptances =
      case Map.get(candidate, :retired) do
        %{} = retired ->
          case matching_scoped_override(repo_policy, candidate, {:retirement, retired}) do
            nil -> []
            override -> [override_acceptance(override)]
          end

        _retired ->
          []
      end

    cooldown_acceptances = cooldown_acceptances(repo_policy, candidate, opts)

    Enum.uniq(advisory_acceptances ++ retirement_acceptances ++ cooldown_acceptances)
  end

  defp cooldown_acceptances(
         %{restriction: %{cooldown: duration}} = repo_policy,
         candidate,
         opts
       )
       when is_binary(duration) do
    now = Keyword.get(opts, :now, System.system_time(:second))
    published_at = Map.get(candidate, :published_at)

    with {:ok, seconds} when seconds > 0 <- Cooldown.duration_to_seconds(duration),
         true <- is_integer(published_at),
         cutoff = {:cutoff, now - seconds, seconds},
         false <- Cooldown.eligible?(published_at, cutoff),
         %{} = override <- matching_scoped_override(repo_policy, candidate, :cooldown) do
      [override_acceptance(override)]
    else
      _other -> []
    end
  end

  defp cooldown_acceptances(_repo_policy, _candidate, _opts), do: []

  defp matching_scoped_override(repo_policy, candidate, finding) do
    repo_policy
    |> Map.get(:overrides, [])
    |> Enum.filter(&valid_override?/1)
    |> Enum.filter(&scoped_override_matches?(&1, candidate, finding))
    |> Enum.sort_by(&override_specificity/1, :desc)
    |> List.first()
  end

  @doc false
  def valid_override?(%{action: action, ref: %{package: package} = ref} = override)
      when is_binary(package) and package != "" do
    action in @override_actions and
      valid_policy_string?(package) and
      valid_requirement?(Map.get(ref, :requirement)) and
      valid_comment?(Map.get(override, :comment)) and
      valid_override_type?(override)
  end

  def valid_override?(_override), do: false

  defp valid_override_type?(override) do
    advisory? = Map.has_key?(override, :advisory_id)
    retirement? = Map.has_key?(override, :retirement_reason)

    case {override.action, advisory?, retirement?} do
      {:OVERRIDE_ACTION_ADVISORY, true, false} ->
        id = Map.get(override, :advisory_id)
        is_binary(id) and valid_policy_string?(id) and String.trim(id) != ""

      {:OVERRIDE_ACTION_RETIREMENT, false, true} ->
        Map.get(override, :retirement_reason) in @retirement_reasons

      {type, false, false}
      when type in [
             :OVERRIDE_ACTION_ALLOW,
             :OVERRIDE_ACTION_DENY,
             :OVERRIDE_ACTION_COOLDOWN
           ] ->
        true

      _other ->
        false
    end
  end

  defp valid_requirement?(nil), do: true
  defp valid_requirement?(""), do: false

  defp valid_requirement?(requirement) when is_binary(requirement) do
    String.valid?(requirement) and
      match?({:ok, _requirement}, Version.parse_requirement(requirement))
  rescue
    _error -> false
  end

  defp valid_requirement?(_requirement), do: false

  defp valid_comment?(nil), do: true

  defp valid_comment?(comment) when is_binary(comment),
    do: valid_policy_string?(comment) and length(String.to_charlist(comment)) <= 500

  defp valid_comment?(_comment), do: false

  defp valid_policy_string?(string) do
    String.valid?(string) and
      Enum.all?(String.to_charlist(string), fn codepoint ->
        Enum.all?(@policy_string_disallowed_ranges, fn {first, last} ->
          codepoint < first or codepoint > last
        end)
      end)
  end

  defp scoped_override_matches?(override, candidate, {:advisory, advisory}) do
    override.action == :OVERRIDE_ACTION_ADVISORY and
      ref_matches?(override.ref, candidate) and
      Hex.Ignores.advisory_matches?(advisory, override.advisory_id)
  end

  defp scoped_override_matches?(override, candidate, {:retirement, retired}) do
    override.action == :OVERRIDE_ACTION_RETIREMENT and
      ref_matches?(override.ref, candidate) and
      Map.get(retired, :reason) == override.retirement_reason
  end

  defp scoped_override_matches?(override, candidate, :cooldown) do
    override.action == :OVERRIDE_ACTION_COOLDOWN and ref_matches?(override.ref, candidate)
  end

  defp override_specificity(%{ref: ref}), do: ref_specificity(ref)

  defp override_acceptance(%{action: :OVERRIDE_ACTION_ADVISORY, advisory_id: id} = override) do
    acceptance(:override, :advisory, id, Map.get(override, :comment))
  end

  defp override_acceptance(
         %{action: :OVERRIDE_ACTION_RETIREMENT, retirement_reason: reason} = override
       ) do
    acceptance(:override, :retirement, reason, Map.get(override, :comment))
  end

  defp override_acceptance(%{action: :OVERRIDE_ACTION_COOLDOWN} = override),
    do: acceptance(:override, :cooldown, nil, Map.get(override, :comment))

  defp override_acceptance(%{action: :OVERRIDE_ACTION_ALLOW} = override),
    do: acceptance(:override, :allow, nil, Map.get(override, :comment))

  defp override_acceptance(%{action: :OVERRIDE_ACTION_DENY} = override),
    do: acceptance(:override, :deny, nil, Map.get(override, :comment))

  defp acceptance(:policy), do: acceptance(:policy, :policy, nil, nil)

  defp acceptance(source, kind, identifier, comment) do
    %{source: source, kind: kind, identifier: identifier, comment: comment}
  end

  defp ref_matches?(%{package: package} = ref, %{package: package} = candidate) do
    case Map.get(ref, :requirement) do
      nil -> true
      requirement -> version_satisfies?(candidate.version, requirement)
    end
  end

  defp ref_matches?(_ref, _candidate), do: false

  defp ref_specificity(ref) do
    case Map.get(ref, :requirement) do
      requirement when is_binary(requirement) and requirement != "" -> 1
      _requirement -> 0
    end
  end

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
  @doc false
  def severity_rank(severity) do
    Enum.find_index(@severity_order, &(&1 == severity)) || length(@severity_order)
  end
end
