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
    advisory = advisory_packages(lock)

    if retired == [] and advisory == [] do
      Hex.Shell.info("No retired or security advisory packages found")
    else
      unless retired == [] do
        header = ["Dependency", "Version", "Retirement reason"]
        Mix.Tasks.Hex.print_table(header, retired)
      end

      unless advisory == [] do
        unless retired == [], do: Hex.Shell.info("")
        print_advisories(advisory)
      end

      Hex.Shell.info("")
      unless retired == [], do: Hex.Shell.error("Found retired packages")
      unless advisory == [], do: Hex.Shell.error("Found packages with security advisories")

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
    retired = Registry.retired(repo, package, version)

    case retired do
      %{} -> [[package, version, Hex.Utils.package_retirement_message(retired)]]
      nil -> []
    end
  end

  defp retirement_status(nil) do
    []
  end

  defp advisory_packages(lock) do
    Enum.flat_map(lock, fn {_app, lock} -> advisory_status(Hex.Utils.lock(lock)) end)
  end

  defp advisory_status(%{repo: repo, name: package, version: version}) do
    advisories = Registry.advisories(repo, package, version) || []

    Enum.map(advisories, fn advisory ->
      {package, version, advisory}
    end)
  end

  defp advisory_status(nil) do
    []
  end

  defp print_advisories(advisories) do
    col_widths =
      Enum.reduce(
        advisories,
        [String.length("Dependency"), String.length("Version")],
        fn {package, version, _adv}, [pw, vw] ->
          [max(pw, byte_size(package)), max(vw, byte_size(version))]
        end
      )

    [pw, vw] = col_widths
    url_prefix = String.duplicate(" ", pw + 2 + vw + 2)

    [[:underline, "Dependency"], [:underline, "Version"], [:underline, "Advisory"]]
    |> print_advisory_row(col_widths)

    Enum.each(advisories, fn {package, version, advisory} ->
      [package, version, Hex.Utils.format_advisory_ansi(advisory, url_prefix)]
      |> print_advisory_row(col_widths)
    end)
  end

  defp print_advisory_row([dep, version, advisory], [pw, vw]) do
    pad = fn str, width ->
      pad_size = width - ansi_length(str) + 2
      [str, :reset, :lists.duplicate(pad_size, ?\s)]
    end

    [pad.(dep, pw), pad.(version, vw), advisory]
    |> IO.ANSI.format()
    |> Hex.Shell.info()
  end

  defp ansi_length(binary) when is_binary(binary), do: byte_size(binary)
  defp ansi_length(list) when is_list(list), do: Enum.reduce(list, 0, &(ansi_length(&1) + &2))
  defp ansi_length(atom) when is_atom(atom), do: 0
end
