defmodule Hex.Mix do
  @moduledoc false

  # Utility functions around Mix dependencies.

  @type deps :: %{String.t() => {boolean, deps}}

  @doc """
  Returns `true` if the version and requirement match.

  See `Version.match?/2`.
  """
  @spec version_match?(String.t(), String.t() | nil) :: boolean
  def version_match?(_version, nil), do: true
  def version_match?(version, req), do: Hex.Version.match?(version, req)

  @doc """
  Given a tree of dependencies return a flat list of all dependencies in
  the tree.

  The returned flattened list is going to contain duplicated dependencies
  because we want to accumulate all of the different requirements.
  However we must skip overridden dependencies as their requirements
  are no longer relevant. We also skip dependencies that are not included
  in the original list of dependencies as they were likely filtered out
  due to options like `:only`.
  """
  @spec flatten_deps([Mix.Dep.t()], [atom]) :: [Mix.Dep.t()]
  def flatten_deps(deps, top_level) do
    apps = Enum.map(deps, & &1.app)
    top_level = Enum.map(top_level, &Atom.to_string/1)
    prepared_deps = prepare_deps(deps)

    deps ++
      for(
        dep <- deps,
        overridden_map = overridden_parents(top_level, prepared_deps, Atom.to_string(dep.app)),
        %{app: app} = child <- dep.deps,
        app in apps and !overridden_map[Atom.to_string(app)],
        do: child
      )
  end

  @doc """
  Returns a map with the overridden upper breadths dependencies of
  the given parent (including the parent level itself).
  """
  @spec overridden_parents([String.t()], deps, String.t()) :: %{String.t() => true}
  def overridden_parents(top_level, deps, parent) do
    deps
    |> Map.take(top_level)
    |> do_overridden_parents(deps, parent, %{})
    |> elem(0)
  end

  def do_overridden_parents(level, deps, parent, visited) do
    {children_map, found?, visited} =
      Enum.reduce(level, {%{}, false, visited}, fn {app, {_override, children}},
                                                   {acc_map, acc_found?, visited} ->
        case Map.fetch(visited, app) do
          {:ok, {children_map, found?}} ->
            {Map.merge(acc_map, children_map), found? or acc_found?, visited}

          :error ->
            children_apps = Map.keys(children)
            children_deps = Map.take(deps, children_apps)

            {children_map, found?, visited} =
              do_overridden_parents(children_deps, deps, parent, visited)

            visited = Map.put(visited, app, {children_map, found?})
            {Map.merge(acc_map, children_map), found? or acc_found?, visited}
        end
      end)

    cond do
      found? ->
        overridden_map = Map.merge(level_to_overridden_map(level), children_map)
        {overridden_map, true, visited}

      Map.has_key?(level, parent) ->
        {level_to_overridden_map(level), true, visited}

      true ->
        {%{}, false, visited}
    end
  end

  defp level_to_overridden_map(level) do
    for {app, {override, _children}} <- level,
        override,
        do: {app, true},
        into: %{}
  end

  @doc """
  Add a potentially new dependency and its children.
  This function is used to add Hex packages to the dependency tree which
  we use in overridden_parents to check overridden status.
  """
  def attach_dep_and_children(deps, app, children) do
    {override, _} = Map.fetch!(deps, app)

    children =
      Enum.into(children, %{}, fn {_repo, _name, app, _req, _optional} ->
        {app, {false, %{}}}
      end)

    Map.merge(children, Map.put(deps, app, {override, children}))
  end

  @doc """
  Converts a list of dependencies to a requests to the resolver. Skips
  dependencies overriding with another SCM (but include dependencies
  overriding with Hex) and dependencies that are not Hex packages.
  """
  def deps_to_requests(deps) do
    requests =
      for %Mix.Dep{app: app, requirement: req, scm: Hex.SCM, opts: opts, from: from} <- deps do
        from = Path.relative_to_cwd(from)
        {opts[:repo], opts[:hex], Atom.to_string(app), req, from}
      end

    # Elixir < 1.3.0-dev returned deps in reverse order
    if Version.compare(System.version(), "1.3.0-dev") == :lt do
      Enum.reverse(requests)
    else
      requests
    end
  end

  @doc """
  Prepare Mix dependencies for the format the resolver expects.
  """
  @spec prepare_deps([Mix.Dep.t()]) :: deps
  def prepare_deps(deps) do
    Enum.into(deps, %{}, fn %Mix.Dep{app: app, deps: deps, opts: opts} ->
      deps =
        Enum.into(deps, %{}, fn %Mix.Dep{app: app, opts: opts} ->
          {Atom.to_string(app), {!!opts[:override], %{}}}
        end)

      {Atom.to_string(app), {!!opts[:override], deps}}
    end)
  end

  @doc """
  Returns all top level dependencies.
  """
  @spec top_level([Mix.Dep.t()]) :: [atom]
  def top_level(deps) do
    deps
    |> Enum.filter(& &1.top_level)
    |> Enum.map(& &1.app)
  end

  @doc """
  Normalises a dependency definition to its 3-tuple form.
  """
  @spec dep(tuple) :: {String.t(), String.t(), Keyword.t()}
  def dep({app, opts}) when is_list(opts), do: {app, nil, opts}
  def dep({app, req}) when is_binary(req), do: {app, req, []}
  def dep({app, req, opts}), do: {app, req, opts}

  @doc """
  Takes all Hex packages from the lock and returns them
  as `{name, app, version, repo}` tuples.
  """
  @spec from_lock(%{}) :: [{String.t(), String.t(), String.t(), String.t()}]
  def from_lock(lock) do
    Enum.flat_map(lock, fn {app, info} ->
      case Hex.Utils.lock(info) do
        %{name: name, version: version, repo: repo} ->
          [{repo, name, Atom.to_string(app), version}]

        nil ->
          []
      end
    end)
  end

  @doc """
  Takes a map of `{name, version}` and returns them as a
  lock of Hex packages.
  """
  def to_lock(result) do
    Enum.into(result, %{}, fn {repo, name, app, version} ->
      inner_checksum =
        Hex.Registry.Server.inner_checksum(repo, name, version)
        |> Base.encode16(case: :lower)

      outer_checksum =
        Hex.Registry.Server.outer_checksum(repo, name, version)
        |> encode_outer_checksum()

      deps =
        Hex.Registry.Server.deps(repo, name, version)
        |> Enum.map(&registry_dep_to_def/1)
        |> Enum.sort()

      managers =
        managers(app)
        |> Enum.sort()
        |> Enum.uniq()

      {String.to_atom(app),
       {:hex, String.to_atom(name), version, inner_checksum, managers, deps, repo, outer_checksum}}
    end)
  end

  defp encode_outer_checksum(nil) do
    nil
  end

  defp encode_outer_checksum(binary) do
    Base.encode16(binary, case: :lower)
  end

  # We need to get managers from manifest if a dependency is not in the lock
  # but it's already fetched. Without the manifest we would only get managers
  # from metadata during checkout or from the lock entry.
  defp managers(nil), do: []

  defp managers(app) do
    path = Path.join([Mix.Project.deps_path(), app, ".hex"])

    case File.read(path) do
      {:ok, file} ->
        case Hex.SCM.parse_manifest(file) do
          {:ok, %{managers: managers}} -> managers
          :error -> []
        end

      _ ->
        []
    end
  end

  defp registry_dep_to_def({repo, name, app, req, optional}) do
    {String.to_atom(app), req, hex: String.to_atom(name), repo: repo, optional: optional}
  end

  def packages_from_lock(lock) do
    Enum.flat_map(lock, fn {_app, info} ->
      case Hex.Utils.lock(info) do
        %{name: name, repo: repo} ->
          [{repo, name}]

        nil ->
          []
      end
    end)
  end

  def normalize_dep({app, opts}) when is_atom(app) and is_list(opts) do
    {app, nil, opts}
  end

  def normalize_dep({app, req}) when is_atom(app) do
    {app, req, []}
  end

  def normalize_dep({app, req, opts}) when is_atom(app) and is_list(opts) do
    {app, req, opts}
  end

  def top_level_deps() do
    config = Mix.Project.config()
    apps_paths = apps_paths(config)
    umbrella_deps = config[:deps] |> Enum.into([], fn deps -> {"", deps} end)

    child_deps =
      Enum.flat_map(apps_paths || [], fn {app, path} ->
        Mix.Project.in_project(app, path, fn _module ->
          Mix.Project.config()[:deps] |> Enum.into([], fn deps -> {path, deps} end)
        end)
      end)

    (umbrella_deps ++ child_deps)
    |> Enum.map(fn {src, dep} -> {src, normalize_dep(dep)} end)
    |> Enum.reduce(%{}, fn {src, {app, req, opts}}, acc ->
      Map.update(acc, app, [{src, req, opts}], &[{src, req, opts} | &1])
    end)
  end

  def apps_paths(config) do
    if apps_path = config[:apps_path] do
      config[:apps] |> umbrella_apps(apps_path) |> to_apps_paths(apps_path)
    end
  end

  defp umbrella_apps(nil, apps_path) do
    case File.ls(apps_path) do
      {:ok, apps} -> Enum.map(apps, &String.to_atom/1)
      {:error, _} -> []
    end
  end

  defp umbrella_apps(apps, _apps_path) when is_list(apps) do
    apps
  end

  defp to_apps_paths(apps, apps_path) do
    for app <- apps,
        path = path_with_mix_exs(app, apps_path),
        do: {app, path},
        into: %{}
  end

  defp path_with_mix_exs(app, apps_path) do
    path = Path.join(apps_path, Atom.to_string(app))

    if File.regular?(Path.join(path, "mix.exs")) do
      path
    end
  end
end
