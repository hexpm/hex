defmodule Hex.Mix do
  @moduledoc false

  # Utility functions around Mix dependencies.

  @type deps :: %{String.t() => {boolean, deps}}

  def overridden_deps(deps) do
    for(
      dep <- deps,
      dep.opts[:override],
      do: dep.app
    )
    |> Enum.uniq()
  end

  @doc """
  Converts a list of dependencies to a requests to the resolver. Skips
  dependencies overriding with another SCM (but include dependencies
  overriding with Hex) and dependencies that are not Hex packages.

  The returned flattened list is going to contain duplicated dependencies
  because we want to accumulate all of the different requirements.
  However we must skip overridden dependencies as their requirements
  are no longer relevant. We also skip dependencies that are not included
  in the original list of dependencies as they were likely filtered out
  due to options like `:only`.
  """
  def deps_to_requests(deps, overridden) do
    apps = Enum.map(deps, & &1.app)

    hex_deps_to_requests(deps, apps, overridden, _top_level? = true) ++
      Enum.flat_map(deps, fn dep ->
        if dep.scm != Hex.SCM and dep.deps != [] do
          [
            %{
              repo: nil,
              name: Atom.to_string(dep.app),
              requirement: nil,
              app: Atom.to_string(dep.app),
              from: Path.relative_to_cwd(dep.from),
              dependencies: hex_deps_to_requests(dep.deps, apps, overridden, _top_level? = false)
            }
          ]
        else
          []
        end
      end)
  end

  defp hex_deps_to_requests(deps, apps, overridden, top_level?) do
    Enum.flat_map(deps, fn dep ->
      if dep.scm == Hex.SCM and dep.app in apps and (dep.top_level or dep.app not in overridden) and
           dep.top_level == top_level? do
        [
          %{
            repo: dep.opts[:repo],
            name: dep.opts[:hex],
            requirement: dep.requirement,
            app: Atom.to_string(dep.app),
            from: Path.relative_to_cwd(dep.from),
            dependencies: []
          }
        ]
      else
        []
      end
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
          [%{repo: repo, name: name, app: Atom.to_string(app), version: version}]

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
    Map.new(result, fn {repo, name, app, version} ->
      inner_checksum =
        Hex.Registry.Server.inner_checksum(repo, name, version)
        |> Base.encode16(case: :lower)

      outer_checksum =
        Hex.Registry.Server.outer_checksum(repo, name, version)
        |> encode_outer_checksum()

      deps =
        Hex.Registry.Server.dependencies(repo, name, version)
        |> case(do: ({:ok, deps} -> deps))
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

  defp registry_dep_to_def(%{
         repo: repo,
         name: name,
         constraint: constraint,
         optional: optional,
         label: app
       }) do
    {String.to_atom(app), to_string(constraint),
     hex: String.to_atom(name), repo: repo || "hexpm", optional: optional}
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
    umbrella_deps = Enum.map(config[:deps], fn deps -> {"", deps} end)

    child_deps =
      Enum.flat_map(apps_paths || [], fn {app, path} ->
        Mix.Project.in_project(app, path, fn _module ->
          Enum.map(Mix.Project.config()[:deps], fn deps -> {path, deps} end)
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
