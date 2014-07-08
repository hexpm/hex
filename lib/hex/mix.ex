defmodule Hex.Mix do
  def version_match?(_version, nil), do: true
  def version_match?(version, req),  do: Version.match?(version, req)

  def deps_to_requests(deps) do
    overridden =
      for %Mix.Dep{app: app, scm: scm, opts: opts} <- deps,
        scm != Hex.SCM and opts[:override],
        do: app

    for %Mix.Dep{app: app, requirement: req, scm: Hex.SCM} <- deps,
        not app in overridden,
        do: {"#{app}", req}
  end

  def deps_to_overridden(deps) do
    for %Mix.Dep{app: app, top_level: true, opts: opts} <- deps,
        opts[:override],
        do: "#{app}"
  end

  def dep({app, opts}) when is_list(opts),
    do: {app, nil, opts}
  def dep({app, req}) when is_binary(req),
    do: {app, req, []}
  def dep({app, req, opts}),
    do: {app, req, opts}

  def from_lock(lock) do
    for {name, {:package, version}} <- lock,
        into: %{},
        do: {"#{name}", version}
  end

  def to_lock(result) do
    for {name, version} <- result,
        into: %{},
        do: {:"#{name}", {:package, version}}
  end
end
