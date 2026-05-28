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

  If a `:cooldown` is configured (see `mix hex.config`) and the latest version of a
  dependency falls within the cooldown window, the row is annotated with `(cooldown)`
  and the version is listed under "Versions in cooldown". Cooldown-held updates do
  not contribute to the non-zero exit code — `mix deps.update` would not pick them
  up until they age out of the window.

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
  package you want to update to not be able to update. If you want to force
  a dependency to be updated to a given version, you can directly update it
  in your `mix.exs`.

  > In a project, this task must be invoked before any other tasks
  > that may load or start your application. Otherwise, you must
  > explicitly list `:hex` as part of your `:extra_applications`.

  ## Command line options

    * `--all` - shows all outdated packages, including children of packages defined in `mix.exs`
    * `--pre` - include pre-releases when checking for newer versions
    * `--within-requirements` - exit with non-zero code only if requirements specified in `mix.exs` is met.
    * `--sort <column>` - sort results by the given column. Currently supports `status`.
    * `--only ANYVALUE` - show only dependencies with the given only value (comma-separated for multiple values)
    * `--json` - output the result as JSON instead of a human readable table. Requires OTP 27 or later.
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [
    all: :boolean,
    pre: :boolean,
    within_requirements: :boolean,
    sort: :string,
    only: :string,
    json: :boolean
  ]

  @impl true
  def run(args) do
    Mix.Tasks.Deps.Loadpaths.run(["--no-compile", "--no-listeners"])
    Hex.start()
    {opts, args} = OptionParser.parse!(args, strict: @switches)
    Registry.open()

    lock = Mix.Dep.Lock.read()

    lock
    |> Hex.Mix.packages_from_lock()
    |> Hex.Registry.Server.prefetch()

    lock
    |> process_lockfile(args, opts)
    |> display_outdated(args, opts)
    |> set_exit_code(args, opts)
  end

  @impl true
  def tasks() do
    [
      {"", "Shows outdated Hex deps for the current project"},
      {"[APP]", "Shows outdated Hex deps for the given dependency"}
    ]
  end

  defp process_lockfile(lock, args, opts) do
    deps = Hex.Mix.top_level_deps()

    deps
    |> requested_dep_names(lock, args, opts)
    |> Enum.sort()
    |> get_versions(deps, lock, opts[:pre])
    |> filter_by_only(args, opts)
  end

  defp requested_dep_names(_deps, lock, [app], _opts) do
    app = String.to_atom(app)

    if is_nil(Hex.Utils.lock(lock[app])) do
      Mix.raise("Dependency #{app} not locked as a Hex package")
    end

    [app]
  end

  defp requested_dep_names(deps, lock, [], opts) do
    if opts[:all], do: Map.keys(lock), else: Map.keys(deps)
  end

  defp requested_dep_names(_deps, _lock, _args, _opts) do
    Mix.raise("""
    Invalid arguments, expected:

    mix hex.outdated [APP]
    """)
  end

  defp display_outdated(versions, args, opts) do
    if opts[:json] do
      versions
      |> Enum.map(&cast_version_map/1)
      |> encode_json!()
      |> Hex.Shell.info()
    else
      display_table(versions, args, opts)
    end

    versions
  end

  defp display_table(
         [{_package, _dep_only, current, latest, requirements, outdated?, cooldown}],
         [_app],
         _opts
       ) do
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

    maybe_print_single_cooldown(latest, cooldown)
  end

  defp display_table(versions, _args, opts) do
    values = versions |> Enum.map(&format_all_row/1) |> maybe_sort_by(opts[:sort])

    diff_links = Enum.map(versions, &build_diff_link/1) |> Enum.reject(&is_nil/1)

    if Enum.empty?(values) do
      Hex.Shell.info("No hex dependencies")
    else
      header = ["Dependency", "Only", "Current", "Latest", "Status"]
      Mix.Tasks.Hex.print_table(header, values)

      maybe_print_cooldown_legend(versions)

      base_message = "Run `mix hex.outdated APP` to see requirements for a specific dependency."
      diff_message = maybe_diff_message(diff_links)
      diff_command_message = maybe_diff_command_message(diff_links)
      Hex.Shell.info(["\n", base_message, diff_message, diff_command_message])
    end
  end

  defp maybe_print_single_cooldown(_latest, nil), do: :ok

  defp maybe_print_single_cooldown(latest, %{eligible_on: eligible_on}) do
    days = Date.diff(eligible_on, Date.utc_today())
    Hex.Shell.info("\nVersion #{latest} is in cooldown — eligible #{eligible_on} (#{days} days)")
  end

  defp maybe_print_cooldown_legend(versions) do
    entries =
      Enum.flat_map(versions, fn
        {package, _, _, latest, _, _, %{eligible_on: eligible_on}} ->
          [{package, latest, eligible_on}]

        _ ->
          []
      end)

    case entries do
      [] ->
        :ok

      entries ->
        today = Date.utc_today()
        Hex.Shell.info("")
        Hex.Shell.info("Versions in cooldown:")

        Enum.each(entries, fn {package, version, eligible_on} ->
          days = Date.diff(eligible_on, today)
          Hex.Shell.info("  #{package} #{version} — eligible #{eligible_on} (#{days} days)")
        end)
    end
  end

  defp set_exit_code(versions, args, opts) do
    outdated_versions =
      Enum.filter(versions, fn {_p, _o, _l, _la, _r, outdated?, cooldown} ->
        outdated? and is_nil(cooldown)
      end)

    if outdated_versions != [] and exit_with_error?(outdated_versions, args, opts) do
      Mix.Tasks.Hex.set_exit_code(1)
    end
  end

  defp exit_with_error?(_outdated, [_app], _opts), do: true

  defp exit_with_error?(outdated, [], opts) do
    !opts[:within_requirements] || any_possible_to_update?(outdated)
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

  defp maybe_sort_by(values, "status") do
    status_order = %{
      "Up-to-date" => 1,
      "Update not possible" => 2,
      "Update possible" => 3
    }

    Enum.sort_by(values, fn [_package, _dep_only, _lock, _latest, [_color, status | _]] ->
      Map.fetch!(status_order, status)
    end)
  end

  defp maybe_sort_by(values, nil) do
    values
  end

  defp filter_by_only(versions, [_app], _opts), do: versions

  defp filter_by_only(versions, [], opts) do
    case opts[:only] do
      nil ->
        versions

      only_value ->
        # deps can have multiple `only` values, so we separate by `,`
        only_values = String.split(only_value, ",", trim: true)

        Enum.filter(versions, fn {_package, dep_only, _lock, _latest, _reqs, _outdated?, _c} ->
          dep_only_parts = String.split(dep_only, ",")
          Enum.any?(dep_only_parts, &(&1 in only_values))
        end)
    end
  end

  defp get_versions(dep_names, deps, lock, pre?) do
    cutoff = Hex.Cooldown.build_cutoff()

    bypass =
      if cutoff == :disabled,
        do: MapSet.new(),
        else: Hex.RemoteConverger.unsafe_lock_bypass(lock)

    Enum.flat_map(dep_names, fn name ->
      case Hex.Utils.lock(lock[name]) do
        %{repo: repo, name: package, version: lock_version} ->
          latest_version = latest_version(repo, package, lock_version, pre?)

          lock_requirements = get_requirements_from_lock(name, lock)
          deps_requirements = get_requirements_from_deps(name, deps)

          outdated? = Version.compare(lock_version, latest_version) == :lt
          requirements = deps_requirements ++ lock_requirements
          dep_only = get_dep_only(deps, name)

          cooldown =
            cond do
              not outdated? -> nil
              not req_matches?(requirements, latest_version) -> nil
              MapSet.member?(bypass, package) -> nil
              true -> cooldown_info(repo, package, latest_version, cutoff)
            end

          [
            {Atom.to_string(name), dep_only, lock_version, latest_version, requirements,
             outdated?, cooldown}
          ]

        _ ->
          []
      end
    end)
  end

  defp cooldown_info(_repo, _package, _version, :disabled), do: nil

  defp cooldown_info(repo, package, version, cutoff) do
    if Hex.Cooldown.repo_excluded?(repo) do
      nil
    else
      published_at = Registry.published_at(repo, package, version)

      if Hex.Cooldown.eligible?(published_at, cutoff) do
        nil
      else
        %{
          published_at: published_at,
          eligible_on: Hex.Cooldown.eligible_on(published_at, cutoff)
        }
      end
    end
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

  defp format_all_row({package, dep_only, lock, latest, requirements, outdated?, cooldown}) do
    latest_color = if outdated?, do: :red, else: :green
    req_matches? = req_matches?(requirements, latest)

    base_status =
      case {outdated?, req_matches?} do
        {true, true} -> [:yellow, "Update possible"]
        {true, false} -> [:red, "Update not possible"]
        {false, _} -> [:green, "Up-to-date"]
      end

    status = if cooldown, do: base_status ++ [" (cooldown)"], else: base_status

    [
      [:bright, package],
      dep_only,
      lock,
      [latest_color, latest],
      status
    ]
  end

  defp build_diff_link({package, _dep_only, lock, latest, requirements, outdated?, _cooldown}) do
    req_matches? = req_matches?(requirements, latest)

    case {outdated?, req_matches?} do
      {true, true} -> "diffs[]=#{package}:#{lock}:#{latest}"
      {_, _} -> nil
    end
  end

  defp version_match?(_version, nil), do: true
  defp version_match?(version, req), do: Version.match?(version, req)

  defp maybe_diff_message([]), do: ""

  defp maybe_diff_message(diff_links) do
    "\n\nTo view the diffs in each available update, visit:\n" <>
      diff_link(diff_links)
  end

  defp maybe_diff_command_message([]), do: ""

  defp maybe_diff_command_message(_diff_links) do
    "\n\nTo view the diff of a specific update, run `mix hex.package diff APP FROM..TO`."
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

  defp any_possible_to_update?(outdated_versions) do
    Enum.any?(outdated_versions, fn {_package, _dep_only, _lock, latest, requirements, _outdated?,
                                     _cooldown} ->
      req_matches?(requirements, latest)
    end)
  end

  defp req_matches?(requirements, latest) do
    Enum.all?(requirements, fn [_source, req_version] -> version_match?(latest, req_version) end)
  end

  defp get_dep_only(deps, dep_name) do
    dep_configs = Map.get(deps, dep_name, [])
    # Get the opts from the first (main project) dependency config
    case List.first(dep_configs) do
      {_src, _req, opts} -> dep_only_from_opts(opts)
      _ -> ""
    end
  end

  defp dep_only_from_opts(opts) do
    Keyword.get(opts, :only, [])
    |> List.wrap()
    |> Enum.map(&to_string/1)
    |> Enum.join(",")
  end

  defp cast_version_map({package, dep_only, lock, latest, requirements, outdated?, cooldown}) do
    %{
      package: package,
      only: dep_only,
      lock_version: lock,
      latest_version: latest,
      requirements:
        Enum.map(requirements, fn [source, req_version] ->
          %{
            source: source,
            requirement: req_version,
            up_to_date: version_match?(latest, req_version)
          }
        end),
      outdated: outdated?,
      cooldown: cast_cooldown_map(cooldown)
    }
  end

  defp cast_cooldown_map(nil), do: nil

  defp cast_cooldown_map(%{published_at: published_at, eligible_on: eligible_on}) do
    %{
      published_at: published_at |> DateTime.from_unix!() |> DateTime.to_iso8601(),
      eligible_on: Date.to_iso8601(eligible_on)
    }
  end

  defp encode_json!(term) do
    if Code.ensure_loaded?(:json) do
      term |> :json.encode() |> IO.iodata_to_binary()
    else
      Mix.raise(":json module is not available, upgrade OTP to use this feature")
    end
  end
end
