defmodule Hex.Policy.Diagnostics do
  @moduledoc false

  alias Hex.Cooldown

  @type filtered_entry :: %{
          repo: String.t(),
          package: String.t(),
          version: String.t(),
          reasons: [term()]
        }

  @versions_listed_per_package 5

  @doc """
  Builds the resolution summary block. Returns `nil` when no policy is loaded;
  with an active policy the block always includes the header line, plus the
  cooldown line and a capped per-version listing of hidden versions when
  relevant.

  `policy` is the decoded policy map (or `nil`); `filtered` is the list of
  `%{repo, package, version, reasons}` entries recorded by Hex.Registry.Policy.
  """
  @spec resolution_summary(map() | nil, [filtered_entry()], String.t() | nil) ::
          String.t() | nil
  def resolution_summary(nil, _filtered, _local_cooldown), do: nil

  def resolution_summary(policy, filtered, local_cooldown) do
    header = "Active policy: #{policy.repository}/#{policy.name}"
    cooldown_line = policy_cooldown_line(policy, local_cooldown)

    hidden_lines =
      if filtered == [] do
        []
      else
        ["Policy hid #{count(length(filtered), "candidate version")}:" | version_lines(filtered)]
      end

    ([header, cooldown_line] ++ hidden_lines)
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  defp version_lines(filtered) do
    filtered
    |> Enum.group_by(fn entry -> {entry.repo, entry.package} end)
    |> Enum.sort()
    |> Enum.flat_map(fn {{_repo, package}, entries} -> package_lines(package, entries) end)
  end

  # Newest-first because the interesting hidden versions are the ones newer
  # than the resolved one; the cap keeps packages with long advisory or
  # retirement histories from flooding the report. `mix hex.policy why` is
  # the uncapped view.
  defp package_lines(package, entries) do
    {listed, rest} =
      entries
      |> Enum.sort_by(&Version.parse!(&1.version), {:desc, Version})
      |> Enum.split(@versions_listed_per_package)

    lines =
      Enum.map(listed, fn entry ->
        attribution = entry.reasons |> Enum.map(&format_reason/1) |> Enum.join(", ")
        "  #{package} #{entry.version} — #{attribution}"
      end)

    case rest do
      [] -> lines
      rest -> lines ++ ["  ...and #{length(rest)} more — run `mix hex.policy why #{package}`"]
    end
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

  @doc """
  Renders a Note: block to append to a solver failure when the active policy
  hid candidate versions. The per-package listing is capped the same way as
  the resolution summary.

  Returns `nil` if there's nothing relevant to say.
  """
  @spec failure_note([filtered_entry()]) :: String.t() | nil
  def failure_note([]), do: nil

  def failure_note(filtered) do
    by_package =
      filtered
      |> Enum.group_by(fn entry -> {entry.repo, entry.package} end)
      |> Enum.sort()

    blocks =
      Enum.map(by_package, fn {{_repo, package}, entries} ->
        "Note: active policy hides #{count(length(entries), "version")} of \"#{package}\":\n" <>
          Enum.join(package_lines(package, entries), "\n")
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

  @doc """
  Maps a blocker reason to an ANSI color used to highlight it in output.
  """
  @spec reason_color(term()) :: atom()
  def reason_color({:advisory, _}), do: :red
  def reason_color({:retirement, _}), do: :yellow
  def reason_color({:cooldown, _, _}), do: :cyan
  def reason_color(:override_deny), do: :red
  def reason_color(_other), do: :reset

  defp count(1, noun), do: "1 #{noun}"
  defp count(n, noun), do: "#{n} #{noun}s"
end
