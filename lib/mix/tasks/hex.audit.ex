defmodule Mix.Tasks.Hex.Audit do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Shows retired Hex deps for the current project"

  @moduledoc """
  Shows all Hex dependencies that have been marked as retired.

  Retired packages are no longer recommended to be used by their
  maintainers. The task will display a message describing
  the reason for retirement and exit with a non-zero code
  if any retired dependencies are found.

  > In a project, this task must be invoked before any other tasks
  > that may load or start your application. Otherwise, you must
  > explicitly list `:hex` as part of your `:extra_applications`.
  """

  @behaviour Hex.Mix.TaskDescription

  @impl true
  def run(_) do
    Mix.Tasks.Deps.Loadpaths.run(["--no-compile"])
    Hex.start()
    Registry.open()

    lock = Mix.Dep.Lock.read()

    lock
    |> Hex.Mix.packages_from_lock()
    |> Registry.prefetch()

    case retired_packages(lock) do
      [] ->
        Hex.Shell.info("No retired packages found")

      packages ->
        header = ["Dependency", "Version", "Retirement reason"]
        Mix.Tasks.Hex.print_table(header, packages)
        Hex.Shell.error("Found retired packages")
        Mix.Tasks.Hex.set_exit_code(1)
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Shows retired Hex deps for the current project"}
    ]
  end

  defp retired_packages(lock) do
    Enum.flat_map(lock, fn {_app, lock} -> retirement_status(Hex.Utils.lock(lock)) end)
  end

  defp retirement_status(%{repo: repo, name: package, version: version}) do
    retired = Registry.retired(repo, package, version)

    case retired do
      %{} -> [[package, version, Hex.Utils.package_retirement_message(retired)]]
      nil -> []
    end
  end

  defp retirement_status(nil) do
    []
  end
end
