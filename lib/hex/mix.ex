defmodule Hex.Mix do
  alias Hex.Registry

  def deps_to_requirements(deps) do
    Enum.flat_map(deps, fn
      Mix.Dep[app: app, opts: opts, requirement: req] ->
        if opts[:package], do: [{ app, req }], else: []
      { app, opts } ->
        if opts[:package], do: [{ app, nil }], else: []
      { app, req, opts } ->
        if opts[:package], do: [{ app, req }], else: []
    end)
  end

  def from_lock(lock) do
    Enum.flat_map(lock, fn { name, opts } ->
      { url, ref } = from_lock_ref(opts)

      case Registry.version_from_ref(name, url, ref) do
        { :ok, version } -> [{ name, version }]
        :error -> []
      end
    end)
  end

  def annotate_deps(result, deps) do
    scms = Mix.SCM.available
    from = Path.absname("mix.exs")

    Enum.map(result, fn { app, version } ->
      { _, _, url, ref } = Registry.get_release(app, version)
      dep = Enum.find(deps, &(elem(&1, 0) == app)) || { app, package: true }
      loaded_dep = Mix.Deps.Loader.to_dep(dep, scms, from)
      loaded_dep.update_opts(&(&1 ++ [git_url: url, git_ref: ref]))
    end)
  end

  defp from_lock_ref({ :git, url, ref, _opts }), do: { url, ref }
end
