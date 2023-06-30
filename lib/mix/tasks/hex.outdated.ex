defmodule Mix.Tasks.Hex.Outdated do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Shows outdated Hex deps for the current project"

  @moduledoc """
  Shows all Hex dependencies that have newer versions in the registry.

      $ mix hex.outdated [APP]

  By default, it only shows top-level packages explicitly listed in the
  `mix.exs` file. All outdated packages can be displayed by using the `--all`
  command line option.

  By default, `hex.outdated` will exit with a non-zero exit code (1) if there are any
  outdated dependencies. You can override this to respect the requirements
  as specified in your `mix.exs` file, with the `--within-requirements` command line option,
  so it only exits with non-zero exit code if the update is possible.

  For example, if your version requirement is "~> 2.0" but the latest version is `3.0`,
  with `--within-requirements` it will exit successfully, but if the latest version
  is `2.8`, then `--within-requirements` will exit with non-zero exit code (1).

  One scenario this could be useful is to ensure you always have the latest
  version of your dependencies, except for major version bumps.

  If a dependency name is given all requirements on that dependency, from
  the entire dependency tree, are listed. This is useful if you are trying
  to figure why a package isn't updating when you run `mix deps.update`.

  Note that when this task determines if a package is updatable it only looks
  at the project's current set of dependency requirements and what version
  they are locked to. When `mix deps.update` is called multiple packages may
  be updated that in turn update their own dependencies, which may cause the
  package you want to update to not be able to update.

  > In a project, this task must be invoked before any other tasks
  > that may load or start your application. Otherwise, you must
  > explicitly list `:hex` as part of your `:extra_applications`.

  ## Command line options

    * `--all` - shows all outdated packages, including children of packages defined in `mix.exs`
    * `--pre` - include pre-releases when checking for newer versions
    * `--within-requirements` - exit with non-zero code only if requirements specified in `mix.exs` is met.
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [all: :boolean, pre: :boolean, within_requirements: :boolean]

  @impl true
  def run(args) do
    Mix.Tasks.Deps.Loadpaths.run(["--no-compile"])
    Hex.start()
    {opts, args} = OptionParser.parse!(args, strict: @switches)
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
    outdated? = Version.compare(current, latest) == :lt
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

    header = ["Source", "Requirement", "Up-to-date"]
    values = Enum.map(requirements, &format_single_row(&1, latest))
    Hex.Shell.info("")
    Mix.Tasks.Hex.print_table(header, values)

    message = "Up-to-date indicates if the requirement matches the latest version."

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
    up_to_date? = if req_matches?, do: "Yes", else: "No"
    [[:bright, source], [req_color, req || ""], [req_color, up_to_date?]]
  end

  defp all(lock, opts) do
    deps = Hex.Mix.top_level_deps()
    dep_names = if opts[:all], do: Map.keys(lock), else: Map.keys(deps)

    versions =
      dep_names
      |> Enum.sort()
      |> get_versions(deps, lock, opts[:pre])

    values = Enum.map(versions, &format_all_row/1)
    diff_links = Enum.map(versions, &build_diff_link/1) |> Enum.reject(&is_nil/1)

    if Enum.empty?(values) do
      Hex.Shell.info("No hex dependencies")
    else
      header = ["Dependency", "Current", "Latest", "Status"]
      Mix.Tasks.Hex.print_table(header, values)

      base_message = "Run `mix hex.outdated APP` to see requirements for a specific dependency."
      diff_message = maybe_diff_message(diff_links)
      Hex.Shell.info(["\n", base_message, diff_message])

      any_outdated? = any_outdated?(versions)
      req_met? = any_req_matches?(versions)

      cond do
        any_outdated? && opts[:within_requirements] && req_met? ->
          Mix.Tasks.Hex.set_exit_code(1)

        any_outdated? && opts[:within_requirements] && not req_met? ->
          nil

        any_outdated? ->
          Mix.Tasks.Hex.set_exit_code(1)

        true ->
          nil
      end
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
    {:ok, default} = Version.parse(default)
    pre? = pre? || default.pre != []
    {:ok, versions} = Registry.versions(repo, package)
    latest = highest_version(versions, pre?)
    to_string(latest || default)
  end

  defp highest_version(versions, pre?) do
    versions =
      if pre? do
        versions
      else
        Enum.filter(versions, fn version -> version.pre == [] end)
      end

    List.last(versions)
  end

  defp format_all_row([package, lock, latest, requirements]) do
    outdated? = Version.compare(lock, latest) == :lt
    latest_color = if outdated?, do: :red, else: :green
    req_matches? = req_matches?(requirements, latest)

    status =
      case {outdated?, req_matches?} do
        {true, true} -> [:yellow, "Update possible"]
        {true, false} -> [:red, "Update not possible"]
        {false, _} -> [:green, "Up-to-date"]
      end

    [
      [:bright, package],
      lock,
      [latest_color, latest],
      status
    ]
  end

  defp build_diff_link([package, lock, latest, requirements]) do
    outdated? = Version.compare(lock, latest) == :lt
    req_matches? = Enum.all?(requirements, &version_match?(latest, &1))

    case {outdated?, req_matches?} do
      {true, true} -> "diffs[]=#{package}:#{lock}:#{latest}"
      {_, _} -> nil
    end
  end

  defp version_match?(_version, nil), do: true
  defp version_match?(version, req), do: Version.match?(version, req)

  defp any_outdated?(versions) do
    Enum.any?(versions, fn [_package, lock, latest, _requirements] ->
      Version.compare(lock, latest) == :lt
    end)
  end

  defp maybe_diff_message([]), do: ""

  defp maybe_diff_message(diff_links) do
    "\n\nTo view the diffs in each available update, visit:\n" <>
      diff_link(diff_links)
  end

  defp diff_link(diff_links) do
    long_url = "https://diff.hex.pm/diffs?" <> Enum.join(diff_links, "&")

    if Hex.State.fetch!(:no_short_urls) do
      long_url
    else
      maybe_get_short_link(long_url)
    end
  end

  defp maybe_get_short_link(long_url) do
    case Hex.API.ShortURL.create(long_url) do
      :error -> long_url
      {:ok, short_url} -> short_url
    end
  end

  defp any_req_matches?(versions) do
    Enum.any?(versions, fn [_package, _lock, latest, requirements] ->
      req_matches?(requirements, latest)
    end)
  end

  defp req_matches?(requirements, latest) do
    Enum.all?(requirements, &version_match?(latest, &1))
  end
end
