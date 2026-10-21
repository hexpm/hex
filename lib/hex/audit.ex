defmodule Hex.Audit do
  @moduledoc false

  # Classifies the retirement, advisory, and policy DENY findings for the Hex
  # packages in a lock. `mix hex.audit` and the `policy_enforce_lock` check
  # that `mix deps.get` and `mix deps.update` run after resolution both use
  # it, so they report the same findings for the same lock.

  alias Hex.Registry.Server, as: Registry

  @type mode :: :default | :overrides | :policy

  @doc """
  Evaluates the Hex packages in `lock`, which must already be prefetched.

  `policy` is `nil` in `:default` mode. The policy is applied first and the
  `ignore_advisories` and `ignore_retirements` configs second. Findings
  accepted by the policy or acknowledged by an ignore entry are returned
  apart from the active ones, and ignore entries never clear a package
  matched by a DENY override. `all_retired` and `raw_advisories` hold every
  finding before classification.
  """
  @spec run(map(), map() | nil, mode()) :: map()
  def run(lock, policy, mode) do
    ignore_advisories = Hex.State.fetch!(:ignore_advisories)
    ignore_retirements = Hex.State.fetch!(:ignore_retirements)

    denied = denied_packages(lock, policy)
    all_retired = retired_packages(lock)
    raw_advisories = advisory_packages(lock)
    all_advisories = display_advisory_findings(raw_advisories)

    {policy_accepted_retired, policy_active_retired} =
      split_policy_findings(all_retired, policy, mode, :retirement)

    {policy_accepted_advisories, policy_active_advisories} =
      split_policy_findings(all_advisories, policy, mode, :advisory)

    {ignored_retired, retired} =
      Enum.split_with(policy_active_retired, fn entry ->
        Hex.Ignores.retirement_ignored?(entry.package, entry.version, ignore_retirements)
      end)

    {ignored_advisories, advisories} =
      Enum.split_with(policy_active_advisories, fn entry ->
        Hex.Ignores.advisory_ignored?(entry.detail, ignore_advisories)
      end)

    %{
      denied: denied,
      retired: retired,
      advisories: advisories,
      policy_accepted_retired: policy_accepted_retired,
      policy_accepted_advisories: policy_accepted_advisories,
      ignored_retired: ignored_retired,
      ignored_advisories: ignored_advisories,
      all_retired: all_retired,
      raw_advisories: raw_advisories
    }
  end

  @doc """
  Returns whether the result has denied, retired, or advisory findings that
  neither the policy nor an ignore entry accepted.
  """
  @spec active_findings?(map()) :: boolean()
  def active_findings?(result) do
    result.denied != [] or result.retired != [] or result.advisories != []
  end

  @doc """
  Raises unless the registry entry of every Hex package in `lock` was fetched
  in this run and includes the locked version. A release without an entry
  would otherwise look like it has no retirement or advisory.

  A cached entry can miss advisories published since it was fetched, so a
  failed fetch fails even when the entry is cached. In offline mode the
  cached entries are used. The `policy_enforce_lock` check of `mix deps.get`
  doesn't call this. It keeps using the cache, and `mix deps.get` has
  already checked the lock against the registry by then.
  """
  @spec ensure_registry_entries!(map()) :: :ok
  def ensure_registry_entries!(lock) do
    unaudited =
      for {_app, info} <- lock,
          %{repo: repo, name: package, version: version} <- [Hex.Utils.lock(info)],
          {:error, reason} <- [registry_entry(repo, package, version)],
          uniq: true,
          do: {Hex.Utils.package_name(repo, package), version, reason}

    if unaudited != [] do
      releases =
        Enum.map_join(unaudited, "\n", fn {package, version, reason} ->
          "  #{package} #{version}: #{unaudited_message(reason)}"
        end)

      # Offline mode can only audit a release whose cached entry includes it
      hint =
        if Enum.all?(unaudited, &match?({_package, _version, {:fetch_failed, true}}, &1)) do
          "\n\nSet HEX_OFFLINE=1 to audit against the local cache instead"
        end

      Mix.raise("Could not audit these locked packages:\n\n#{releases}#{hint}")
    end

    :ok
  end

  defp registry_entry(repo, package, version) do
    includes_version? =
      case Registry.versions(repo, package) do
        {:ok, versions} -> version in Enum.map(versions, &to_string/1)
        :error -> false
      end

    cond do
      Registry.fetch_failed?(repo, package) -> {:error, {:fetch_failed, includes_version?}}
      includes_version? -> :ok
      Hex.State.fetch!(:offline) -> {:error, :not_cached}
      true -> {:error, :not_in_registry}
    end
  end

  defp unaudited_message({:fetch_failed, _includes_version?}),
    do: "the registry entry could not be fetched"

  defp unaudited_message(:not_cached), do: "the locked version is not cached locally"
  defp unaudited_message(:not_in_registry), do: "the locked version is not in the registry"

  defp denied_packages(_lock, nil), do: []

  defp denied_packages(lock, policy) do
    Enum.flat_map(lock, fn {_app, lock} -> denied_status(Hex.Utils.lock(lock), policy) end)
  end

  defp denied_status(%{repo: repo, name: package, version: version}, policy) do
    candidate = Hex.Policy.Filter.candidate_from_registry(repo, package, version)

    case Hex.Policy.Filter.deny_override(policy, candidate) do
      nil -> []
      acceptance -> [finding(repo, package, version, acceptance)]
    end
  end

  defp denied_status(nil, _policy), do: []

  defp retired_packages(lock) do
    Enum.flat_map(lock, fn {_app, lock} -> retirement_status(Hex.Utils.lock(lock)) end)
  end

  defp retirement_status(%{repo: repo, name: package, version: version}) do
    case Registry.retired(repo, package, version) do
      %{} = retired ->
        [finding(repo, package, version, retired)]

      nil ->
        []
    end
  end

  defp retirement_status(nil), do: []

  defp advisory_packages(lock) do
    Enum.flat_map(lock, fn {_app, lock} -> advisory_status(Hex.Utils.lock(lock)) end)
  end

  defp advisory_status(%{repo: repo, name: package, version: version}) do
    case Registry.advisories(repo, package, version) || [] do
      [] ->
        []

      advisories ->
        [%{repo: repo || "hexpm", package: package, version: version, advisories: advisories}]
    end
  end

  defp advisory_status(nil), do: []

  defp display_advisory_findings(raw_advisories) do
    Enum.flat_map(raw_advisories, fn entry ->
      groups = Enum.group_by(entry.advisories, &advisory_group_key/1)

      group_keys =
        entry.advisories
        |> Enum.map(&advisory_group_key/1)
        |> Enum.uniq()

      display_advisories = :mix_hex_advisory.group_for_display(entry.advisories)

      Enum.zip(display_advisories, group_keys)
      |> Enum.map(fn {advisory, group_key} ->
        policy_advisory =
          groups
          |> Map.fetch!(group_key)
          |> Enum.max_by(
            &Hex.Policy.Filter.severity_rank(Map.get(&1, :severity)),
            fn -> advisory end
          )

        finding(entry.repo, entry.package, entry.version, advisory)
        |> Map.put(
          :policy_detail,
          Map.put(advisory, :severity, Map.get(policy_advisory, :severity))
        )
      end)
    end)
  end

  defp advisory_group_key(%{id: id} = advisory) do
    Enum.find([id | Map.get(advisory, :aliases, [])], &String.starts_with?(&1, "CVE-")) || id
  end

  defp split_policy_findings(entries, nil, :default, _type), do: {[], entries}

  defp split_policy_findings(entries, policy, mode, type) do
    {accepted, active} =
      Enum.split_with(entries, fn entry ->
        candidate =
          Hex.Policy.Filter.candidate_from_registry(entry.repo, entry.package, entry.version)

        detail = Map.get(entry, :policy_detail, entry.detail)

        case Hex.Policy.Filter.audit_finding(policy, candidate, {type, detail}, mode) do
          :active -> false
          {:accepted, _acceptance} -> true
        end
      end)

    accepted =
      Enum.map(accepted, fn entry ->
        candidate =
          Hex.Policy.Filter.candidate_from_registry(entry.repo, entry.package, entry.version)

        detail = Map.get(entry, :policy_detail, entry.detail)

        {:accepted, acceptance} =
          Hex.Policy.Filter.audit_finding(policy, candidate, {type, detail}, mode)

        %{entry | acceptance: acceptance}
      end)

    {accepted, active}
  end

  defp finding(repo, package, version, detail) do
    %{repo: repo || "hexpm", package: package, version: version, detail: detail, acceptance: nil}
  end

  @doc """
  Prints audit sections, each a `{type, header, entries, policy_accepted?}`
  tuple where `type` is `:denied`, `:retired`, or `:advisories`. Empty
  sections are skipped.
  """
  def print_sections(sections) do
    sections
    |> Enum.reject(fn {_type, _header, entries, _policy_accepted?} -> entries == [] end)
    |> Enum.with_index()
    |> Enum.each(fn {{type, header, entries, policy_accepted?}, index} ->
      if index > 0, do: Hex.Shell.info("")
      print_section(type, header, entries, policy_accepted?)
    end)
  end

  defp print_section(:denied, header, entries, _policy_accepted?) do
    Hex.Shell.info(Hex.Shell.format([:bright, header, :reset]))

    Enum.each(entries, fn entry ->
      message = Hex.Policy.Filter.acceptance_message(entry.detail)

      Hex.Shell.info(
        Hex.Shell.format(["  #{entry.package} #{entry.version} - ", :red, message, :reset])
      )
    end)
  end

  defp print_section(:retired, header, entries, policy_accepted?) do
    Hex.Shell.info(Hex.Shell.format([:bright, header, :reset]))

    Enum.each(entries, fn entry ->
      message = Hex.Utils.package_retirement_message(entry.detail)

      Hex.Shell.info(
        Hex.Shell.format(["  #{entry.package} #{entry.version} - ", :yellow, message, :reset])
      )

      if policy_accepted?, do: print_policy_acceptance(entry.acceptance, "    ")
    end)
  end

  defp print_section(:advisories, header, entries, policy_accepted?) do
    Hex.Shell.info(Hex.Shell.format([:bright, header, :reset]))

    entries
    |> Enum.with_index()
    |> Enum.each(fn {entry, index} ->
      if index > 0, do: Hex.Shell.info("")

      Hex.Shell.info(
        Hex.Shell.format([
          "  #{entry.package} #{entry.version} - "
          | Hex.Utils.format_advisory_ansi(entry.detail, "    ")
        ])
      )

      if policy_accepted?, do: print_policy_acceptance(entry.acceptance, "    ")
    end)
  end

  defp print_policy_acceptance(acceptance, indent) do
    explanation = Hex.Policy.Filter.acceptance_message(acceptance)
    Hex.Shell.info(Hex.Shell.format([:green, "#{indent}Policy: #{explanation}", :reset]))
  end
end
