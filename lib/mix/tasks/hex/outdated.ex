defmodule Mix.Tasks.Hex.Outdated do
  use Mix.Task

  @shortdoc "Shows outdated Hex deps for the current project"

  @moduledoc """
  Shows all packages that have a version mismatch between the registry and
  your mix.lock file.

  By default, it only shows top-level packages explicitly listed in the
  `mix.exs` file. All outdated packages can be displayed by using the `--all`
  command line option.

  If a dependency name is given all requirements on that dependency, from
  the entire dependency tree, are listed. This is useful if you are trying
  to figure why a package isn't updating when you run `mix deps.update`.

  ## Command line options

    * `--all` - shows all outdated packages, including children of packages defined in `mix.exs`
    * `--pre` - include pre-releases when checking for newer versions

  `mix hex.outdated [APP]`
  """

  @recursive true
  @switches [all: :boolean, pre: :boolean]

  def run(args) do
    {opts, args, _} = OptionParser.parse(args, switches: @switches)
    Hex.start
    Hex.Utils.ensure_registry!()

    lock = Mix.Dep.Lock.read
    deps = Mix.Dep.loaded([]) |> Enum.filter(& &1.scm == Hex.SCM)

    # Re-open registry because loading deps cleans the process dict
    Hex.Utils.ensure_registry!()

    case args do
      [app] ->
        single(deps, lock, app, opts)
      [] ->
        all(deps, lock, opts)
    end
  end

  defp single(deps, lock, app, opts) do
    app = String.to_atom(app)
    dep = Enum.find(deps, &(&1.app == app))

    unless dep do
      Mix.raise "No dependency with name #{app}"
    end

    {package, current} =
      case Hex.Utils.lock(lock[app]) do
        [:hex, package, lock_version, _checksum, _managers, _deps] ->
          {package, lock_version}
        _ ->
          Mix.raise "Dependency #{app} not locked as a Hex package"
      end

    latest       = latest_version(package, current, opts[:pre])
    outdated?    = Hex.Version.compare(current, latest) == :lt
    requirements = get_requirements(sort(deps), app)

    requirements =
      if dep.top_level do
        [["mix.exs", dep.requirement]|requirements]
      else
        requirements
      end

    if outdated? do
      ["There is newer version of the dependency available ", :bright, latest, " > ", current, :reset, "!"]
      |> IO.ANSI.format
      |> Hex.Shell.info
    else
      ["Current version ", :bright, current, :reset, " of dependency is up to date!"]
      |> IO.ANSI.format
      |> Hex.Shell.info
    end

    Hex.Shell.info ""

    header = ["Parent", "Requirement"]
    requirements
    |> print_results(header, &print_header/1, &print_single_row(&1, latest))

    Hex.Shell.info ""
    Hex.Shell.info "A green requirement means that it matches the latest version."
  end

  defp get_requirements(deps, app) do
    Enum.flat_map(deps, fn dep ->
      Enum.find_value(dep.deps, fn child ->
        if child.app == app do
          [[Atom.to_string(dep.app), child.requirement]]
        end
      end) || []
    end)
  end

  defp print_single_row([{parent, p1}, {req, _p2}], latest) do
    req_matches? = version_match?(latest, req)
    req = req || ""
    req_color = if req_matches?, do: :green, else: :red

    [:bright, parent, :reset, p1, req_color, req]
    |> IO.ANSI.format
    |> Hex.Shell.info
  end

  defp all(deps, lock, opts) do
    header = ["Dependency", "Current", "Latest", "Requirement"]

    if opts[:all] do
      deps
    else
      Enum.filter(deps, & &1.top_level)
    end
    |> sort
    |> get_versions(lock, opts[:pre])
    |> print_results(header, &print_header/1, &print_all_row/1)

    Hex.Shell.info ""
    Hex.Shell.info "A green version in latest means you have the latest " <>
                   "version of a given package. A green requirement means " <>
                   "your current requirement matches the latest version."
  end

  defp sort(deps) do
    Enum.sort(deps, &(&1.app <= &2.app))
  end

  defp get_versions(deps, lock, pre?) do
    Enum.flat_map(deps, fn dep ->
      case Hex.Utils.lock(lock[dep.app]) do
        [:hex, package, lock_version, _checksum, _managers, _deps] ->
          latest_version = latest_version(package, lock_version, pre?)
          req = dep.requirement
          [[Atom.to_string(dep.app), lock_version, latest_version, req]]
        _ ->
          []
      end
    end)
  end

  defp latest_version(package, default, pre?) do
    {:ok, default} = Hex.Version.parse(default)
    pre? = pre? || default.pre != []

    latest =
      package
      |> Atom.to_string
      |> Hex.Registry.get_versions
      |> highest_version(pre?)

    latest || default
  end

  defp highest_version(versions, pre?) do
    versions = if pre? do
      versions
    else
      Enum.filter(versions, fn version ->
        {:ok, version} = Hex.Version.parse(version)
        version.pre == []
      end)
    end

    List.last(versions)
  end

  defp print_results(rows, header, print_header, print_row) do
    table  = [header|rows]
    widths = widths(table)

    print_header.(pad_row(header, widths))
    Enum.each(rows, &print_row.(pad_row(&1, widths)))
  end

  defp pad_row(row, widths) do
    Enum.map(Enum.zip(row, widths), fn {string, width} ->
      {string, :binary.copy(" ", width-size(string) + 2)}
    end)
  end

  defp print_header(row) do
    row
    |> Enum.map(fn {str, pad} -> [:underline, str, :reset, pad] end)
    |> IO.ANSI.format
    |> Hex.Shell.info
  end

  defp print_all_row([{package, p1}, {lock, p2}, {latest, p3}, {req, _p4}]) do
    outdated? = Hex.Version.compare(lock, latest) == :lt
    latest_color = if outdated?, do: :red, else: :green

    req_matches? = version_match?(latest, req)
    req = req || ""
    req_color = if req_matches?, do: :green, else: :red

    [:bright, package, :reset, p1,
     lock, p2,
     latest_color, latest, :reset, p3,
     req_color, req]
    |> IO.ANSI.format
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
  defp version_match?(version, req),  do: Hex.Version.match?(version, req)
end
