defmodule Mix.Tasks.Hex.Audit do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  def run(_) do
    Hex.start

    lock = Mix.Dep.Lock.read
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
    Enum.map(deps,
      fn dep -> retirement_status(Hex.Utils.lock(lock[dep.app])) end)
    |> Enum.reject(&Enum.empty?/1)
  end

  defp retirement_status(%{repo: repo, name: package, version: version}) do
    retired = Registry.retired(repo, package, version)

    case retired do
      %{} -> [package, version, Hex.Utils.package_retirement_message(retired)]
      nil -> []
    end
  end
  defp retirement_status(_), do: []
end
