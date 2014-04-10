defmodule Hex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  @registry_updated :registry_updated

  def remote?(dep) do
    !! dep.opts[:hex_app]
  end

  def converge(main, old_lock) do
    unless File.exists?(Hex.Registry.path()) do
      if Hex.Util.update_registry("Fetching registry...") == :error do
        raise Mix.Error
      end
    end

    Hex.Registry.start

    apps      = Enum.map(main, &(&1.app))
    lock      = Enum.reject(old_lock, fn { app, _ } -> app in apps end)
    locked    = Hex.Mix.from_lock(lock)
    reqs      = Hex.Mix.deps_to_requests(main)
    overriden = Hex.Mix.overriden(main)

    print_info(reqs, locked)

    if resolved = Hex.Resolver.resolve(reqs, overriden, locked) do
      print_success(resolved, locked)
      new_lock = Hex.Mix.to_lock(resolved)
      Dict.merge(old_lock, new_lock)
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
        Mix.shell.info "  #{dep} : v#{version}"
      end)
    end
  end
end
