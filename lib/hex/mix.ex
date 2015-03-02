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
  Converts a list of dependencies to a requests to the resolver. Skips
  dependencies overriding with another SCM (but include dependencies
  overriding with Hex) and dependencies that are not Hex packages.
  """
  @spec deps_to_requests([Mix.Dep.t]) :: [{String.t, String.t}]
  def deps_to_requests(deps) do
    for %Mix.Dep{app: app, requirement: req, scm: Hex.SCM, opts: opts} <- deps,
        do: {Atom.to_string(opts[:hex]), Atom.to_string(app), req}
  end

  @doc """
  Returns all top level dependencies.
  """
  @spec top_level([Mix.Dep.t]) :: [Mix.Dep.t]
  def top_level(deps) do
    Enum.filter(deps, & &1.top_level)
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
        [{Atom.to_string(app), Atom.to_string(name), version}]
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
