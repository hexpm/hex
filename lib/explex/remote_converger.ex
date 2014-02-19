defmodule Explex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  def remote?({ app, _opts }) do
    remote?(app)
  end

  def remote?(app) when is_atom(app) do
    Explex.Registry.package_exists?(app)
  end

  def converge(main) do
    # TODO: Warn when requests are not in the registry

    deps   = Mix.project[:deps] || []
    lock   = Mix.Deps.Lock.read
    locked = Explex.Mix.from_lock(lock)
    reqs   = Explex.Mix.deps_to_requirements(main)

    print_info(reqs, locked)

    if resolved = Explex.Resolver.resolve(reqs, locked) do
      print_success(resolved)
      Explex.Mix.annotate_deps(resolved, deps)
    else
      raise Mix.Error, message: "Dependency resolution failed. Relax the version requirements."
    end
  end

  defp print_info(reqs, locked) do
    resolve = Enum.filter_map(reqs,
        fn { req, _ } -> not Keyword.has_key?(locked, req) end,
        &elem(&1, 0))

    if resolve != [] do
      Mix.shell.info "Running dependency resolution for unlocked dependencies..."
      Mix.shell.info inspect(resolve, pretty: true)
    end
  end

  defp print_success(resolved) do
    Mix.shell.info "Dependency resolution completed successfully"
    Enum.each(resolved, fn { dep, version } ->
      Mix.shell.info "    #{dep} : #{version}"
    end)
  end
end
