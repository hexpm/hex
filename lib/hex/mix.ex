defmodule Hex.Mix do
  @moduledoc """
  Utility functions around Mix dependencies.
  """

  @doc """
  Returns `true` if the version and requirement match.

  See `Version.match?/2`.
  """
  @spec version_match?(String.t, String.t) :: boolean
  def version_match?(_version, nil), do: true
  def version_match?(version, req),  do: Hex.Version.match?(version, req)

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
  @spec flatten_deps([Mix.Dep.t], [atom]) :: [Mix.Dep.t]
  def flatten_deps(deps, top_level) do
    deps ++
      for(dep <- deps,
          overridden_map = overridden_parents(top_level, deps, dep.app),
          %{app: app} = child <- dep.deps,
          !overridden_map[app],
          do: child)
  end

  @doc """
  Returns a map with the overridden upper breadths dependencies of
  the given parent (including the parent level itself).
  """
  @spec overridden_parents([atom], [Mix.Dep.t], atom) :: [Mix.Dep.t]
  def overridden_parents(top_level, deps, parent) do
    deps
    |> Enum.filter(&(&1.app in top_level))
    |> do_overridden_parents(deps, parent)
    |> elem(0)
  end

  def do_overridden_parents(level, deps, parent) do
    {children_maps, found?} =
      Enum.map_reduce(level, false, fn dep, acc_found? ->
        children_apps = Enum.map(dep.deps, & &1.app)
        children_deps = Enum.filter(deps, & &1.app in children_apps)
        {children_map, found?} = do_overridden_parents(children_deps, deps, parent)
        {children_map, found? or acc_found?}
      end)

    cond do
      found? ->
        maps = [level_to_overridden_map(level)|children_maps]
        {Enum.reduce(maps, &Map.merge/2), true}
      parent in Enum.map(level, & &1.app) ->
        {level_to_overridden_map(level), true}
      true ->
        {%{}, false}
    end
  end

  defp level_to_overridden_map(level) do
    for %{app: app, opts: opts} <- level,
        opts[:override],
        do: {app, true},
        into: %{}
  end

  @doc """
  Add a potentially new dependency and its children.
  This function is used to add Hex packages to the dependency tree which
  we use in overridden_parents to check overridden status.
  """
  def attach_dep_and_children(deps, app, children) do
    app = String.to_atom(app)
    dep = Enum.find(deps, &(&1.app == app))

    children =
      Enum.map(children, fn {name, app, _req, _optional} ->
        app = String.to_atom(app)
        name = String.to_atom(name)
        %Mix.Dep{app: app, opts: [hex: name]}
      end)

    new_dep = %{dep | deps: children}

    put_dep(deps, new_dep) ++ children
  end

  # Replace a dependency in the tree
  defp put_dep(deps, new_dep) do
    app = new_dep.app
    Enum.map(deps, fn dep ->
      if dep.app == app, do: new_dep, else: dep
    end)
  end

  @doc """
  Converts a list of dependencies to a requests to the resolver. Skips
  dependencies overriding with another SCM (but include dependencies
  overriding with Hex) and dependencies that are not Hex packages.
  """
  @spec deps_to_requests([Mix.Dep.t]) :: [{String.t, String.t}]
  def deps_to_requests(deps) do
    for %Mix.Dep{app: app, requirement: req, scm: Hex.SCM, opts: opts, from: from} <- deps do
      from = Path.relative_to_cwd(from)
      {Atom.to_string(opts[:hex]), Atom.to_string(app), req, from}
    end
  end

  @doc """
  Returns all top level dependencies.
  """
  @spec top_level([Mix.Dep.t]) :: [atom]
  def top_level(deps) do
    Enum.filter_map(deps, & &1.top_level, & &1.app)
  end

  @doc """
  Normalises a dependency definition to its 3-tuple form.
  """
  @spec dep(tuple) :: {String.t, String.t, Keyword.t}
  def dep({app, opts}) when is_list(opts),
    do: {app, nil, opts}
  def dep({app, req}) when is_binary(req),
    do: {app, req, []}
  def dep({app, req, opts}),
    do: {app, req, opts}

  @doc """
  Takes all Hex packages from the lock and returns them
  as `{name, version}` tuples.
  """
  @spec from_lock(%{}) :: [{String.t, String.t, String.t}]
  def from_lock(lock) do
    Enum.flat_map(lock, fn
      {app, {:hex, name, version}} ->
        [{Atom.to_string(name), Atom.to_string(app), version}]
      _ ->
        []
    end)
  end

  @doc """
  Takes a map of `{name, version}` and returns them as a
  lock of Hex packages.
  """
  @spec to_lock(%{}) :: %{}
  def to_lock(result) do
    Enum.into(result, %{}, fn {name, app, version} ->
      {String.to_atom(app), {:hex, String.to_atom(name), version}}
    end)
  end
end
