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

  If you have overridden a version of a dependency, it may appear in this output.
  You need to change the version in your override, otherwise `mix deps.update`
  will not update that dependency.

  ## Command line options

    * `--all` - shows all outdated packages, including children of packages defined in `mix.exs`
    * `--pre` - include pre-releases when checking for newer versions
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [all: :boolean, pre: :boolean]

  @impl true
  def run(args) do
    Hex.check_deps()
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)
    Registry.open()

    lock = Mix.Dep.Lock.read()

    lock
    |> Hex.Mix.packages_from_lock()
    |> Hex.Registry.Server.prefetch()

    case args do
      [app] ->
        single(lock, app, opts)

      [] ->
        all(lock, opts)

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.outdated [APP]
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Shows outdated Hex deps for the current project"},
      {"[APP]", "Shows outdated Hex deps for the given dependency"}
    ]
  end

  defp single(lock, app, opts) do
    app = String.to_atom(app)
    deps = Hex.Mix.top_level_deps()

    {repo, package, current} =
      case Hex.Utils.lock(lock[app]) do
        %{repo: repo, name: package, version: version} ->
          {repo, package, version}

        nil ->
          Mix.raise("Dependency #{app} not locked as a Hex package")
      end

    latest = latest_version(repo, package, current, opts[:pre])
    outdated? = Hex.Version.compare(current, latest) == :lt
    lock_requirements = get_requirements_from_lock(app, lock)
    deps_requirements = get_requirements_from_deps(app, deps)
    requirements = deps_requirements ++ lock_requirements

    if outdated? do
      [
        "There is newer version of the dependency available ",
        [:bright, latest, " > ", current, :reset, "!"]
      ]
      |> IO.ANSI.format_fragment()
      |> Hex.Shell.info()
    else
      ["Current version ", :bright, current, :reset, " of dependency is up to date!"]
      |> IO.ANSI.format_fragment()
      |> Hex.Shell.info()
    end

    header = ["Source", "Requirement"]
    values = Enum.map(requirements, &format_single_row(&1, latest))
    Hex.Shell.info("")
    Mix.Tasks.Hex.print_table(header, values)

    message = "A green requirement means that it matches the latest version."
    Hex.Shell.info(["\n", message])

    if outdated?, do: Mix.Tasks.Hex.set_exit_code(1)
  end

  defp get_requirements_from_lock(app, lock) do
    Enum.flat_map(lock, fn {source, lock} ->
      case Hex.Utils.lock(lock) do
        %{deps: nil} ->
          []

        %{deps: deps} ->
          Enum.flat_map(deps, fn {dep_app, req, _opts} ->
            if app == dep_app, do: [[Atom.to_string(source), req]], else: []
          end)

        nil ->
          []
      end
    end)
  end

  defp get_requirements_from_deps(app, deps) do
    # TODO: Path to umbrella child's mix.exs

    case Map.fetch(deps, app) do
      {:ok, deps} ->
        Enum.map(deps, fn {src, req, _opts} -> [Path.join([src, "mix.exs"]), req] end)

      :error ->
        []
    end
  end

  defp format_single_row([source, req], latest) do
    req_matches? = version_match?(latest, req)
    req_color = if req_matches?, do: :green, else: :red
    [[:bright, source], [req_color, req || ""]]
  end

  defp all(lock, opts) do
    deps = Hex.Mix.top_level_deps()
    dep_names = if opts[:all], do: Map.keys(lock), else: Map.keys(deps)

    versions =
      dep_names
      |> Enum.sort()
      |> get_versions(deps, lock, opts[:pre])

    values = Enum.map(versions, &format_all_row/1)

    if Enum.empty?(values) do
      Hex.Shell.info("No hex dependencies")
    else
      header = ["Dependency", "Current", "Latest", "Update possible"]
      Mix.Tasks.Hex.print_table(header, values)

      message =
        "A green version in latest means you have the latest " <>
          "version of a given package. Update possible indicates " <>
          "if your current requirement matches the latest version.\n" <>
          "Run `mix hex.outdated APP` to see requirements for a specific dependency."

      Hex.Shell.info(["\n" | message])
      if any_outdated?(versions), do: Mix.Tasks.Hex.set_exit_code(1)
    end
  end

  defp get_versions(dep_names, deps, lock, pre?) do
    Enum.flat_map(dep_names, fn name ->
      case Hex.Utils.lock(lock[name]) do
        %{repo: repo, name: package, version: lock_version} ->
          latest_version = latest_version(repo, package, lock_version, pre?)

          lock_requirements = get_requirements_from_lock(name, lock)
          deps_requirements = get_requirements_from_deps(name, deps)

          requirements =
            (deps_requirements ++ lock_requirements)
            |> Enum.map(fn [_, req_version] -> req_version end)

          [[Atom.to_string(name), lock_version, latest_version, requirements]]

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
    versions =
      if pre? do
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

    req_matches? = Enum.all?(requirements, &version_match?(latest, &1))

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
  defp version_match?(version, req), do: Hex.Version.match?(version, req)

  defp any_outdated?(versions) do
    Enum.any?(versions, fn [_package, lock, latest, _requirements] ->
      Hex.Version.compare(lock, latest) == :lt
    end)
  end
end
