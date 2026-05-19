defmodule Mix.Tasks.Hex.Audit do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Shows retired Hex deps and security advisories for the current project"

  @moduledoc """
  Shows all Hex dependencies that have been marked as retired or have
  security advisories.

  Retired packages are no longer recommended to be used by their
  maintainers. The task will display a message describing
  the reason for retirement and exit with a non-zero code
  if any retired dependencies are found.

  Security advisories indicate known vulnerabilities in a package version.
  The task will display advisory details and exit with a non-zero code
  if any packages with advisories are found.

  > In a project, this task must be invoked before any other tasks
  > that may load or start your application. Otherwise, you must
  > explicitly list `:hex` as part of your `:extra_applications`.
  """

  @behaviour Hex.Mix.TaskDescription

  @impl true
  def run(_) do
    Mix.Tasks.Deps.Loadpaths.run(["--no-compile", "--no-listeners"])
    Hex.start()
    Registry.open()

    lock = Mix.Dep.Lock.read()

    lock
    |> Hex.Mix.packages_from_lock()
    |> Registry.prefetch()

    retired = retired_packages(lock)
    advisories = advisory_packages(lock)

    if retired == [] and advisories == [] do
      Hex.Shell.info("No retired or security advisory packages found")
    else
      if retired != [], do: print_retired_section(retired)

      if advisories != [] do
        if retired != [], do: Hex.Shell.info("")
        print_advisories_section(advisories)
      end

      Hex.Shell.info("")
      if retired != [], do: Hex.Shell.error("Found retired packages")
      if advisories != [], do: Hex.Shell.error("Found packages with security advisories")

      Mix.Tasks.Hex.set_exit_code(1)
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Shows retired Hex deps and security advisories for the current project"}
    ]
  end

  defp retired_packages(lock) do
    Enum.flat_map(lock, fn {_app, lock} -> retirement_status(Hex.Utils.lock(lock)) end)
  end

  defp retirement_status(%{repo: repo, name: package, version: version}) do
    case Registry.retired(repo, package, version) do
      %{} = retired -> [{package, version, Hex.Utils.package_retirement_message(retired)}]
      nil -> []
    end
  end

  defp retirement_status(nil), do: []

  defp advisory_packages(lock) do
    Enum.flat_map(lock, fn {_app, lock} -> advisory_status(Hex.Utils.lock(lock)) end)
  end

  defp advisory_status(%{repo: repo, name: package, version: version}) do
    advisories = Registry.advisories(repo, package, version) || []
    Enum.map(advisories, fn advisory -> {package, version, advisory} end)
  end

  defp advisory_status(nil), do: []

  defp print_retired_section(retired) do
    Hex.Shell.info(Hex.Shell.format([:bright, "Retired:", :reset]))

    Enum.each(retired, fn {package, version, message} ->
      Hex.Shell.info(Hex.Shell.format(["  #{package} #{version} - ", :yellow, message, :reset]))
    end)
  end

  defp print_advisories_section(advisories) do
    Hex.Shell.info(Hex.Shell.format([:bright, "Advisories:", :reset]))

    advisories
    |> Enum.with_index()
    |> Enum.each(fn {{package, version, advisory}, index} ->
      if index > 0, do: Hex.Shell.info("")

      Hex.Shell.info(
        Hex.Shell.format([
          "  #{package} #{version} - " | Hex.Utils.format_advisory_ansi(advisory, "    ")
        ])
      )
    end)
  end
end
