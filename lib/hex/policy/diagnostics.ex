defmodule Hex.Policy.Diagnostics do
  @moduledoc false

  alias Hex.Cooldown

  @type filtered_entry :: %{
          repo: String.t(),
          package: String.t(),
          version: String.t(),
          blockers: [%{policy: map(), reason: term()}]
        }

  @doc """
  Builds the resolution summary block. Returns `nil` when no policies
  are loaded or nothing was filtered.

  `policies` is a list of decoded policy maps; `filtered` is the
  list of `%{repo, package, version, blockers}` entries recorded by
  Hex.Registry.Policy.
  """
  @spec resolution_summary([map()], [filtered_entry()], String.t() | nil) ::
          String.t() | nil
  def resolution_summary([], _filtered, _local_cooldown), do: nil

  def resolution_summary(policies, filtered, local_cooldown) do
    refs =
      policies
      |> Enum.map(fn p -> "#{p.repository}/#{p.name}" end)
      |> Enum.sort()

    header = "Active policies: #{Enum.join(refs, ", ")} (#{length(policies)})"

    cooldown_line = effective_cooldown_line(policies, local_cooldown)

    hidden_line =
      if filtered != [] do
        "Policies hid #{length(filtered)} candidate versions"
      end

    per_policy_lines = per_policy_breakdown(filtered, policies)

    [header, cooldown_line, hidden_line | per_policy_lines]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  @doc """
  Computes the effective cooldown across the local configuration and every
  active policy, returning `{source, duration}` or `nil` when zero.

  Each policy contributes the strictest cooldown across its repository tabs.
  """
  @spec effective_cooldown([map()], String.t() | nil) ::
          {:local | {String.t(), String.t()}, String.t()} | nil
  def effective_cooldown(policies, local_cooldown) do
    case Cooldown.strictest([{:local, local_cooldown} | policy_cooldowns(policies)]) do
      {_source, "0d"} -> nil
      result -> result
    end
  end

  @doc """
  Returns `{tag, duration}` cooldown candidates from each policy, where the
  duration is the strictest across that policy's repository tabs and the tag
  is `{repository, name}`.
  """
  @spec policy_cooldowns([map()]) :: [{{String.t(), String.t()}, String.t()}]
  def policy_cooldowns(policies) do
    Enum.map(policies, fn p ->
      durations =
        for rp <- Map.get(p, :repositories, []),
            restriction = Map.get(rp, :restriction),
            restriction != nil,
            duration = Map.get(restriction, :cooldown),
            is_binary(duration),
            do: duration

      {_tag, strictest} =
        Cooldown.strictest([{nil, "0d"} | Enum.map(durations, &{nil, &1})])

      {{p.repository, p.name}, strictest}
    end)
  end

  defp effective_cooldown_line(policies, local_cooldown) do
    case effective_cooldown(policies, local_cooldown) do
      nil ->
        nil

      {source, duration} ->
        "Effective cooldown: #{duration} (#{cooldown_source(source)})"
    end
  end

  defp cooldown_source(:local), do: "local"
  defp cooldown_source({repo, name}), do: "#{repo}/#{name}"

  defp per_policy_breakdown(filtered, policies) do
    for p <- policies do
      reasons =
        for entry <- filtered,
            blocker <- entry.blockers,
            blocker.policy.repository == p.repository and blocker.policy.name == p.name,
            do: blocker.reason

      if reasons != [] do
        "  #{p.repository}/#{p.name}: #{length(reasons)} (#{reason_breakdown(reasons)})"
      end
    end
    |> Enum.reject(&is_nil/1)
  end

  defp reason_breakdown(reasons) do
    reasons
    |> Enum.frequencies_by(&reason_category/1)
    |> Enum.sort()
    |> Enum.map(fn {category, count} -> "#{count} #{category}" end)
    |> Enum.join(", ")
  end

  defp reason_category({:advisory, _}), do: "advisory"
  defp reason_category({:retirement, _}), do: "retirement"
  defp reason_category({:cooldown, _, _}), do: "cooldown"
  defp reason_category(:override_deny), do: "override deny"

  @doc """
  Renders a Note: block to append to a solver failure when active
  policies hid candidate versions.

  Returns `nil` if there's nothing relevant to say.
  """
  @spec failure_note([filtered_entry()]) :: String.t() | nil
  def failure_note([]), do: nil

  def failure_note(filtered) do
    by_package = Enum.group_by(filtered, fn entry -> {entry.repo, entry.package} end)

    blocks =
      Enum.map(by_package, fn {{_repo, package}, entries} ->
        lines =
          Enum.map(entries, fn entry ->
            attribution = entry.blockers |> Enum.map(&format_blocker/1) |> Enum.join(", ")
            "  #{package} #{entry.version} — #{attribution}"
          end)

        "Note: active policies hide #{length(entries)} versions of \"#{package}\":\n" <>
          Enum.join(lines, "\n")
      end)

    Enum.join(blocks, "\n\n")
  end

  @doc """
  Formats a `Hex.Policy.load_all/0` error for `Mix.raise/1`.
  """
  @spec format_load_error(term()) :: String.t()
  def format_load_error(:invalid_policy_config) do
    "Policy configuration is invalid. Check the `:policy` key in mix.exs, " <>
      "the HEX_POLICY env var, and `mix hex.config policy`."
  end

  def format_load_error(other), do: "Policy loading failed: #{inspect(other)}"

  @doc """
  Formats a single blocker's reason with its responsible policy.
  """
  @spec format_blocker(%{policy: map(), reason: term()}) :: String.t()
  def format_blocker(%{policy: p, reason: reason}) do
    "#{p.repository}/#{p.name} (#{format_reason(reason)})"
  end

  @doc """
  Formats a blocker reason without policy attribution.
  """
  @spec format_reason(term()) :: String.t()
  def format_reason({:advisory, sev}), do: "advisory ≥ #{Hex.Utils.advisory_severity(sev)}"

  def format_reason({:retirement, r}),
    do: "retirement: #{Hex.Utils.package_retirement_reason(r)}"

  def format_reason({:cooldown, duration, eligible_on}),
    do: "cooldown #{duration}; eligible #{eligible_on}"

  def format_reason(:override_deny), do: "override deny"

  def format_reason(other), do: inspect(other)
end
