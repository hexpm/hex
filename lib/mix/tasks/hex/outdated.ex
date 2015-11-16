defmodule Mix.Tasks.Hex.Outdated do
  use Mix.Task

  @shortdoc "Shows outdated Hex deps for the current project"

  @moduledoc """
  Shows all packages that have a version mismatch between the registry and
  your mix.lock file.

  By default, it only shows top-level packages explicitly listed in the
  `mix.exs` file. All outdated packages can be displayed by using the `--all`
  command line option.

  ## Command line options

    * `--all` - shows all outdated packages, including packages in `mix.lock` not specified in `mix.exs`.

  `mix hex.outdated`
  `mix hex.outdated --all`
  """

  @recursive true

  def run(args) do
    {opts, _args, _} = OptionParser.parse(args, switches: [all: :boolean])
    Hex.start
    Hex.Utils.ensure_registry!()

    lock = Mix.Dep.Lock.read
    deps = Mix.Dep.loaded([]) |> Enum.filter(& &1.scm == Hex.SCM)

    if opts[:all] do
      deps
    else
      Enum.filter(deps, & &1.top_level)
    end
    |> get_versions(lock)
    |> print_results

    Hex.Shell.info ""
    Hex.Shell.info "A green version in latest means you have the latest " <>
                   "version of a given package. A green requirement means " <>
                   "your current requirement matches the latest version."
  end

  defp get_versions(deps, lock) do
    Enum.flat_map(deps, fn dep ->
      case lock[dep.app] do
        {:hex, package, lock_version} ->
          latest_version =
            package
            |> Atom.to_string
            |> Hex.Registry.get_versions
            |> List.last

          req = dep.requirement

          [[Atom.to_string(dep.app), lock_version, latest_version, req]]
        _ ->
          []
      end
    end)
  end

  defp print_results([]) do
    Mix.shell.info "All packages are up-to-date"
  end

  defp print_results(packages) do
    header = ["Dependency", "Latest", "Current", "Requirement"]
    table  = [header|packages]
    widths = widths(table)

    print_header(pad_row(header, widths))
    Enum.each(packages, &print_row(pad_row(&1, widths)))
  end

  defp pad_row(row, widths) do
    Enum.map(Enum.zip(row, widths), fn {string, width} ->
      {string, :binary.copy(" ", width-size(string)+2)}
    end)
  end

  defp print_header(row) do
    row
    |> Enum.map(fn {str, pad} -> [:underline, str, :reset, pad] end)
    |> IO.ANSI.format
    |> Hex.Shell.info
  end

  defp print_row([{package, p1}, {lock, p2}, {latest, p3}, {req, _p4}]) do
    outdated? = Version.compare(lock, latest) == :lt
    lock_color = if outdated?, do: :red, else: :green

    req_matches? = version_match?(latest, req)
    req = req || ""
    req_color = if req_matches?, do: :green, else: :red

    [:bright, package, :reset, p1, lock_color, lock, :reset, p2,
     latest, p3, req_color, req, :reset]
    |>IO.ANSI.format
    |> Hex.Shell.info
  end

  defp widths([head|tail]) do
    widths = Enum.map(head, &size/1)

    Enum.reduce(tail, widths, fn list, acc ->
      Enum.zip(list, acc)
      |> Enum.map(fn {string, width} -> max(width, size(string)) end)
    end)
  end

  defp size(nil), do: 0
  defp size(str), do: byte_size(str)

  defp version_match?(_version, nil), do: true
  defp version_match?(version, req),  do: Version.match?(version, req)
end
