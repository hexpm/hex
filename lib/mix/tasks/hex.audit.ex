defmodule Mix.Tasks.Hex.Audit do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Shows retired Hex dependencies"

  @moduledoc """
  Shows all Hex dependencies that have been marked as retired.

  Retired packages are no longer recommended to be used by their
  maintainers. The task will display a message describing
  the reason for retirement and exit with a non-zero code
  if any retired dependencies are found.
  """

  def run(_) do
    Hex.start

    lock = Mix.Dep.Lock.read()
    deps = Mix.Dep.loaded([]) |> Enum.filter(& &1.scm == Hex.SCM)

    Registry.open

    Hex.Mix.packages_from_lock(lock)
    |> Registry.prefetch

    case retired_packages(deps, lock) do
      [] ->
        Hex.Shell.info "No retired packages found"
      packages ->
        header = ["Dependency", "Version", "Retirement reason"]
        Mix.Tasks.Hex.print_table(header, packages)
        Mix.raise "Found retired packages"
    end
  end

  defp retired_packages(deps, lock) do
    Enum.map(deps, &retirement_status(Hex.Utils.lock(lock[&1.app])))
    |> Enum.reject(&Enum.empty?/1)
  end

  defp retirement_status(%{repo: repo, name: package, version: version}) do
    retired = Registry.retired(repo, package, version)

    case retired do
      %{} -> [package, version, Hex.Utils.package_retirement_message(retired)]
      nil -> []
    end
  end
end
