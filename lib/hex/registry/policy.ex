defmodule Hex.Registry.Policy do
  @moduledoc false

  @behaviour Hex.Solver.Registry

  alias Hex.Registry.Cooldown
  alias Hex.Policy.Filter

  @impl true
  defdelegate prefetch(packages), to: Cooldown

  @impl true
  defdelegate dependencies(repo, package, version), to: Cooldown

  @impl true
  def versions(repo, package) do
    case Cooldown.versions(repo, package) do
      {:ok, versions} ->
        case Hex.State.fetch!(:active_policy) do
          nil -> {:ok, versions}
          policy -> {:ok, filter(versions, repo, package, policy)}
        end

      :error ->
        :error
    end
  end

  defp filter(versions, repo, package, policy) do
    if Filter.restricts_repo?(policy, repo) do
      locked = locked_versions(repo, package)
      policy = Filter.for_package(policy, package)

      {allowed, blocked} =
        Enum.reduce(versions, {[], []}, fn version, {allowed, blocked} ->
          if to_string(version) in locked do
            {[version | allowed], blocked}
          else
            candidate = Filter.candidate_from_registry(repo, package, version)

            case Filter.classify(policy, candidate) do
              :allowed -> {[version | allowed], blocked}
              {:blocked, reasons} -> {allowed, [{version, reasons} | blocked]}
            end
          end
        end)

      record_blocks(repo, package, Enum.reverse(blocked))
      Enum.reverse(allowed)
    else
      versions
    end
  end

  # Versions already in the lockfile are trusted at install and exempt from
  # policy filtering, so re-resolution keeps a locked-but-now-blocked entry.
  defp locked_versions(repo, package) do
    Hex.State.fetch!(:policy_locked_versions)
    |> Map.get({repo || "hexpm", package}, [])
  end

  # Another resolution in the same run asks for the package's versions again,
  # so the same blocked version can come through here again; record it once.
  defp record_blocks(_repo, _package, []), do: :ok

  defp record_blocks(repo, package, blocked) do
    blocked_entries =
      Enum.map(blocked, fn {version, reasons} ->
        %{repo: repo || "hexpm", package: package, version: to_string(version), reasons: reasons}
      end)

    Hex.State.update!(:policy_filtered_versions, fn entries ->
      Enum.reduce(blocked_entries, entries, fn entry, entries ->
        if entry in entries, do: entries, else: [entry | entries]
      end)
    end)
  end
end
