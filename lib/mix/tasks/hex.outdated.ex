defmodule Mix.Tasks.Hex.Outdated do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Shows outdated Hex deps for the current project"

  @moduledoc """
  Shows all Hex dependencies that have newer versions in the registry.

  By default, it only shows top-level packages explicitly listed in the
  `mix.exs` file. All outdated packages can be displayed by using the `--all`
  command line option.

  If a dependency name is given all requirements on that dependency, from
  the entire dependency tree, are listed. This is useful if you are trying
  to figure why a package isn't updating when you run `mix deps.update`.

      mix hex.outdated [APP]

  ## Command line options

    * `--all` - shows all outdated packages, including children of packages defined in `mix.exs`
    * `--pre` - include pre-releases when checking for newer versions
  """

  @switches [all: :boolean, pre: :boolean]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    lock = Mix.Dep.Lock.read()
    deps = Mix.Dep.loaded([]) |> Enum.filter(& &1.scm == Hex.SCM)

    Hex.Registry.Server.open()
    Hex.Mix.packages_from_lock(lock)
    |> Hex.Registry.Server.prefetch()

    case args do
      [app] ->
        single(deps, lock, app, opts)
      [] ->
        all(deps, lock, opts)
      _ ->
        Mix.raise """
        Invalid arguments, expected:

        mix hex.outdated [APP]
        """
    end
  end

  defp single(deps, lock, app, opts) do
    app = String.to_atom(app)
    dep = Enum.find(deps, &(&1.app == app))

    unless dep do
      Mix.raise "No dependency with name #{app}"
    end

    {repo, package, current} =
      case Hex.Utils.lock(lock[app]) do
        %{repo: repo, name: package, version: version} ->
          {repo, package, version}
        nil ->
          Mix.raise "Dependency #{app} not locked as a Hex package"
      end

    latest = latest_version(repo, package, current, opts[:pre])
    outdated? = Hex.Version.compare(current, latest) == :lt
    requirements = get_requirements(sort(deps), app)

    requirements =
      if dep.top_level do
        [["mix.exs", dep.requirement] | requirements]
      else
        requirements
      end

    if outdated? do
      ["There is newer version of the dependency available ", :bright, latest, " > ", current, :reset, "!"]
      |> IO.ANSI.format_fragment()
      |> Hex.Shell.info()
    else
      ["Current version ", :bright, current, :reset, " of dependency is up to date!"]
      |> IO.ANSI.format_fragment()
      |> Hex.Shell.info()
    end

    header = ["Source", "Requirement"]
    values = Enum.map(requirements, &format_single_row(&1, latest))
    Hex.Shell.info ""
    Mix.Tasks.Hex.print_table(header, values)

    message = "A green requirement means that it matches the latest version."
    Hex.Shell.info ["\n", message]
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

  defp format_single_row([source, req], latest) do
    req_matches? = version_match?(latest, req)
    req_color = if req_matches?, do: :green, else: :red
    [[:bright, source], [req_color, req || ""]]
  end

  defp all(deps, lock, opts) do
    values =
      if(opts[:all], do: deps, else: Enum.filter(deps, & &1.top_level))
      |> sort()
      |> get_versions(lock, opts[:pre])
      |> Enum.map(&format_all_row/1)

    if Enum.empty?(values) do
      Hex.Shell.info "No hex dependencies"
    else
      header = ["Dependency", "Current", "Latest", "Update possible"]
      Mix.Tasks.Hex.print_table(header, values)

      message =
        "A green version in latest means you have the latest " <>
        "version of a given package. Update possible indicates " <>
        "if your current requirement matches the latest version.\n" <>
        "Run `mix hex.outdated APP` to see requirements for a specific dependency."
      Hex.Shell.info ["\n" | message]
    end
  end

  defp sort(deps) do
    Enum.sort(deps, &(&1.app <= &2.app))
  end

  defp get_versions(deps, lock, pre?) do
    Enum.flat_map(deps, fn dep ->
      case Hex.Utils.lock(lock[dep.app]) do
        %{repo: repo, name: package, version: lock_version} ->
          latest_version = latest_version(repo, package, lock_version, pre?)

          requirements =
            deps
            |> get_requirements(dep.app)
            |> Enum.map(fn [_, req_version] -> req_version end)
          requirements = [dep.requirement | requirements]

          [[Atom.to_string(dep.app), lock_version, latest_version, requirements]]
        _ ->
          []
      end
    end)
  end

  defp latest_version(repo, package, default, pre?) do
    {:ok, default} = Hex.Version.parse(default)
    pre? = pre? || default.pre != []

    latest =
      Registry.versions(repo, package)
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

  defp format_all_row([package, lock, latest, requirements]) do
    outdated? = Hex.Version.compare(lock, latest) == :lt
    latest_color = if outdated?, do: :red, else: :green

    req_matches? = Enum.all?(requirements, &(version_match?(latest, &1)))

    {update_possible_color, update_possible} =
      case {outdated?, req_matches?} do
        {true, true} -> {:green, "Yes"}
        {true, false} -> {:red, "No"}
        {false, _} -> {:green, ""}
      end

    [
      [:bright, package],
      lock,
      [latest_color, latest],
      [update_possible_color, update_possible]
    ]
  end

  defp version_match?(_version, nil), do: true
  defp version_match?(version, req),  do: Hex.Version.match?(version, req)
end
