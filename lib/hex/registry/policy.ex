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
        policies = Map.values(Hex.State.fetch!(:policies))

        if policies == [] do
          {:ok, versions}
        else
          {:ok, filter(versions, repo, package, policies)}
        end

      :error ->
        :error
    end
  end

  defp filter(versions, repo, package, policies) do
    locked = locked_versions(repo, package)

    Enum.filter(versions, fn version ->
      if to_string(version) in locked do
        true
      else
        candidate = Filter.candidate_from_registry(repo, package, version)

        case Filter.classify_set(policies, candidate) do
          :allowed ->
            true

          {:blocked, blockers} ->
            record_block(repo, package, version, blockers)
            false
        end
      end
    end)
  end

  # Versions already in the lockfile are trusted at install and exempt from
  # policy filtering, so re-resolution keeps a locked-but-now-blocked entry.
  defp locked_versions(repo, package) do
    Hex.State.fetch!(:policy_locked_versions)
    |> Map.get({repo || "hexpm", package}, [])
  end

  defp record_block(repo, package, version, blockers) do
    entry = %{
      repo: repo || "hexpm",
      package: package,
      version: to_string(version),
      blockers: blockers
    }

    Hex.State.update!(:policy_filtered_versions, &[entry | &1])
  end
end
