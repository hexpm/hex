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
  def version_match?(version, req),  do: Version.match?(version, req)

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
  @spec flatten_deps([Mix.Dep.t]) :: [Mix.Dep.t]
  def flatten_deps(deps) do
    apps = Enum.map(deps, & &1.app)
    top_level = top_level(deps)

    deps ++
      for(dep <- deps,
          upper_breadths = down_to(top_level, deps, dep.app),
          %{app: app} = child <- dep.deps,
          app in apps and not was_overridden?(upper_breadths, app),
          do: child)
  end

  @doc """
  Returns all the upper breadths for the parent of a dependency.
  """
  @spec down_to([atom], [Mix.Dep.t], atom) :: [Mix.Dep.t]
  def down_to(top_level, deps, parent) do
    Enum.filter(deps, &(&1.app in top_level))
    |> do_down_to(deps, parent)
  end

  def do_down_to(level, deps, parent) do
    children =
      Enum.flat_map(level, fn dep ->
        children = Enum.map(dep.deps, & &1.app)
        children = Enum.filter(deps, fn dep -> dep.app in children end)
        do_down_to(children, deps, parent)
      end)

    cond do
      children != [] ->
        level ++ children
      parent in Enum.map(level, & &1.app) ->
        level
      true ->
        []
    end
  end

  @doc """
  Returns true if the dependency was overridden in any of the upper breadths.
  """
  @spec was_overridden?([Mix.Dep.t], atom) :: boolean
  def was_overridden?(upper_breadths, app) do
    Enum.any?(upper_breadths, fn dep ->
      app == dep.app && dep.opts[:override]
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
