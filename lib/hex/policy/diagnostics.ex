defmodule Hex.Policy.Diagnostics do
  @moduledoc false

  alias Hex.Cooldown

  @type filtered_entry :: %{
          repo: String.t(),
          package: String.t(),
          version: String.t(),
          reasons: [term()]
        }

  @doc """
  Builds the resolution summary block. Returns `nil` when no policy is loaded;
  with an active policy the block always includes the header line, plus the
  cooldown and hidden-version lines when relevant.

  `policy` is the decoded policy map (or `nil`); `filtered` is the list of
  `%{repo, package, version, reasons}` entries recorded by Hex.Registry.Policy.
  """
  @spec resolution_summary(map() | nil, [filtered_entry()], String.t() | nil) ::
          String.t() | nil
  def resolution_summary(nil, _filtered, _local_cooldown), do: nil

  def resolution_summary(policy, filtered, local_cooldown) do
    ref = "#{policy.repository}/#{policy.name}"
    header = "Active policy: #{ref}"

    cooldown_line = policy_cooldown_line(policy, local_cooldown)

    hidden_line =
      if filtered != [] do
        "Policy hid #{count(length(filtered), "candidate version")}"
      end

    breakdown_line =
      if filtered != [] do
        reasons = for entry <- filtered, reason <- entry.reasons, do: reason
        "  #{ref}: #{length(reasons)} (#{reason_breakdown(reasons)})"
      end

    [header, cooldown_line, hidden_line, breakdown_line]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  @doc """
  Computes the effective cooldown across the local configuration and the active
  policy, returning `{source, duration}` or `nil` when zero.

  The policy contributes the strictest cooldown across its repository tabs.
  """
  @spec effective_cooldown(map() | nil, String.t() | nil) ::
          {:local | {String.t(), String.t()}, String.t()} | nil
  def effective_cooldown(policy, local_cooldown) do
    case Cooldown.strictest([{:local, local_cooldown} | policy_cooldown(policy)]) do
      {_source, "0d"} -> nil
      result -> result
    end
  end

  # Returns a single `{{repo, name}, duration}` cooldown candidate for the
  # policy, where the duration is the strictest across its repository tabs, or
  # an empty list when the policy sets no cooldown.
  defp policy_cooldown(nil), do: []

  defp policy_cooldown(policy) do
    durations =
      for rp <- Map.get(policy, :repositories, []),
          restriction = Map.get(rp, :restriction),
          restriction != nil,
          duration = Map.get(restriction, :cooldown),
          is_binary(duration),
          do: duration

    case durations do
      [] ->
        []

      _ ->
        {_tag, strictest} =
          Cooldown.strictest([{nil, "0d"} | Enum.map(durations, &{nil, &1})])

        [{{policy.repository, policy.name}, strictest}]
    end
  end

  # A locally configured cooldown gets its own "Versions filtered by cooldown"
  # report after resolution, so the policy summary only calls out a cooldown
  # the policy itself imposes.
  defp policy_cooldown_line(policy, local_cooldown) do
    case effective_cooldown(policy, local_cooldown) do
      {{repo, name}, duration} ->
        "Effective cooldown: #{duration} (#{repo}/#{name})"

      _ ->
        nil
    end
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
  Renders a Note: block to append to a solver failure when the active policy
  hid candidate versions.

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
            attribution = entry.reasons |> Enum.map(&format_reason/1) |> Enum.join(", ")
            "  #{package} #{entry.version} — #{attribution}"
          end)

        "Note: active policy hides #{count(length(entries), "version")} of \"#{package}\":\n" <>
          Enum.join(lines, "\n")
      end)

    Enum.join(blocks, "\n\n")
  end

  @doc """
  Formats a blocker reason.
  """
  @spec format_reason(term()) :: String.t()
  def format_reason({:advisory, sev}), do: "advisory ≥ #{Hex.Utils.advisory_severity(sev)}"

  def format_reason({:retirement, r}),
    do: "retirement: #{Hex.Utils.package_retirement_reason(r)}"

  def format_reason({:cooldown, duration, eligible_on}),
    do: "cooldown #{duration}; eligible #{eligible_on}"

  def format_reason(:override_deny), do: "override deny"

  def format_reason(other), do: inspect(other)

  defp count(1, noun), do: "1 #{noun}"
  defp count(n, noun), do: "#{n} #{noun}s"
end
