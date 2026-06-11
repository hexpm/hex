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
    locked = locked_versions(repo, package)

    Enum.filter(versions, fn version ->
      if to_string(version) in locked do
        true
      else
        candidate = Filter.candidate_from_registry(repo, package, version)

        case Filter.classify(policy, candidate) do
          :allowed ->
            true

          {:blocked, reasons} ->
            record_block(repo, package, version, reasons)
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

  # The solver may ask for a package's versions once per requirement, so the
  # same blocked version comes through here repeatedly; record it once.
  defp record_block(repo, package, version, reasons) do
    entry = %{
      repo: repo || "hexpm",
      package: package,
      version: to_string(version),
      reasons: reasons
    }

    Hex.State.update!(:policy_filtered_versions, fn entries ->
      if entry in entries, do: entries, else: [entry | entries]
    end)
  end
end
