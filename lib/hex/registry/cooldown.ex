defmodule Hex.Registry.Cooldown do
  @moduledoc false

  @behaviour Hex.Solver.Registry

  alias Hex.Registry.Server

  @impl true
  defdelegate prefetch(packages), to: Server

  @impl true
  defdelegate dependencies(repo, package, version), to: Server

  @impl true
  def versions(repo, package) do
    case Server.versions(repo, package) do
      {:ok, versions} ->
        cutoff = Hex.State.fetch!(:cooldown_cutoff)
        bypass = Hex.State.fetch!(:cooldown_bypass_packages)

        cond do
          cutoff == :disabled ->
            {:ok, versions}

          Hex.Cooldown.repo_excluded?(repo) ->
            {:ok, versions}

          MapSet.member?(bypass, package) ->
            {:ok, versions}

          true ->
            locked =
              Map.get(Hex.State.fetch!(:cooldown_locked_versions), {repo || "hexpm", package}, [])

            {:ok, filter(versions, repo, package, cutoff, locked)}
        end

      :error ->
        :error
    end
  end

  defp filter(versions, repo, package, cutoff, locked) do
    {eligible, filtered_out} =
      Enum.split_with(versions, fn version ->
        version_str = to_string(version)

        if version_str in locked do
          true
        else
          published_at = Server.published_at(repo, package, version_str)
          Hex.Cooldown.eligible?(published_at, cutoff)
        end
      end)

    record_filtered(repo, package, filtered_out)
    eligible
  end

  defp record_filtered(_repo, _package, []), do: :ok

  defp record_filtered(repo, package, versions) do
    repo_key = repo || "hexpm"

    entries =
      Enum.map(versions, fn version ->
        version_str = to_string(version)
        {repo_key, package, version_str, Server.published_at(repo, package, version_str)}
      end)

    Hex.State.update!(:cooldown_filtered_versions, &(entries ++ &1))
  end
end
