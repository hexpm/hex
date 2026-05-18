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
            {:ok, filter(versions, repo, package, cutoff)}
        end

      :error ->
        :error
    end
  end

  defp filter(versions, repo, package, cutoff) do
    Enum.filter(versions, fn version ->
      published_at = Server.published_at(repo, package, to_string(version))
      Hex.Cooldown.eligible?(published_at, cutoff)
    end)
  end
end
