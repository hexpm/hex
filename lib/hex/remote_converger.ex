defmodule Hex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  def remote?({ app, _opts }) do
    remote?(app)
  end

  def remote?(app) when is_atom(app) do
    Hex.Registry.package_exists?("#{app}")
  end

  def converge(deps) do
    main      = Mix.project[:deps] || []
    lock      = Mix.Deps.Lock.read
    locked    = Hex.Mix.from_lock(lock)
    reqs      = Hex.Mix.deps_to_requests(deps)
    overriden = Hex.Mix.overriden(main)

    check_requests(reqs)
    print_info(reqs, locked)

    if resolved = Hex.Resolver.resolve(reqs, overriden, locked) do
      print_success(resolved, locked)
      Hex.Mix.annotate_deps(resolved, deps)
    else
      raise Mix.Error, message: "Dependency resolution failed, relax the version requirements or unlock dependencies"
    end
  end

  defp print_info(reqs, locked) do
    resolve =
      Enum.flat_map(reqs, fn { app, _req} ->
        if Dict.has_key?(locked, app), do: [], else: [app]
      end)

    if resolve != [] do
      Mix.shell.info "Running dependency resolution for unlocked dependencies: " <> Enum.join(resolve, ", ")
    end
  end

  defp print_success(resolved, locked) do
    resolved = Dict.drop(resolved, Dict.keys(locked))
    if resolved != [] do
      Mix.shell.info "Dependency resolution completed successfully"
      Enum.each(resolved, fn { dep, version } ->
        Mix.shell.info "    #{dep} : #{version}"
      end)
    end
  end

  defp check_requests(reqs) do
    Enum.each(reqs, fn { package, _req } ->
      unless Hex.Registry.package_exists?(package) do
        raise Mix.Error, message: "Package #{package} not found in registry"
      end
    end)
  end
end
