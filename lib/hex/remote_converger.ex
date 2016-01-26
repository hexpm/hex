defmodule Hex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  alias Hex.Registry

  def remote?(dep) do
    !!dep.opts[:hex]
  end

  def converge(deps, lock) do
    Hex.start
    Hex.Utils.ensure_registry!()

    verify_lock(lock)

    # We cannot use given lock here, because all deps that are being
    # converged have been removed from the lock by Mix
    # We need the old lock to get the children of Hex packages
    old_lock = Mix.Dep.Lock.read

    locked    = prepare_locked(lock, old_lock, deps)
    top_level = Hex.Mix.top_level(deps)
    flat_deps = Hex.Mix.flatten_deps(deps, top_level)
    reqs      = Hex.Mix.deps_to_requests(flat_deps)

    check_deps(deps, top_level)
    check_input(reqs, locked)

    Hex.Shell.info "Running dependency resolution"

    case Hex.Resolver.resolve(reqs, deps, top_level, locked) do
      {:ok, resolved} ->
        print_success(resolved, locked)
        new_lock = Hex.Mix.to_lock(resolved)
        Hex.SCM.prefetch(new_lock)
        Map.merge(lock, new_lock)

      {:error, message} ->
        Hex.Shell.error message
        Mix.raise "Hex dependency resolution failed, relax the version requirements or unlock dependencies"
    end
  after
    Hex.Registry.clean_pdict
  end

  def deps(%Mix.Dep{app: app}, lock) do
    case Map.fetch(lock, app) do
      {:ok, {:hex, name, version}} ->
        get_deps(name, version)
      _ ->
        []
    end
  end

  defp get_deps(name, version) do
    case Hex.Registry.open() do
      :ok ->
        name = Atom.to_string(name)
        deps = Registry.get_deps(name, version) || []

        for {name, app, req, optional} <- deps do
          app = String.to_atom(app)
          opts = [optional: optional, hex: String.to_atom(name)]

          # Support old packages where requirement could be missing
          if req do
            {app, req, opts}
          else
            {app, opts}
          end
        end

      {:error, reason} ->
        if File.exists?(Hex.Registry.path) do
          Hex.Shell.warn("Failed to open Hex registry file (#{inspect reason})")
        else
          Hex.Shell.warn("Missing Hex registry file, run `mix hex.info` to fetch")
        end
        []
    end
  end

  defp check_deps(deps, top_level) do
    Enum.each(deps, fn dep ->
      if dep.app in top_level and
         is_nil(dep.requirement) and
         dep.scm == Hex.SCM do
        Hex.Shell.warn "#{dep.app} is missing its version requirement, " <>
                       "use \">= 0.0.0\" if it should match any version"
      end
    end)
  end

  defp check_input(reqs, locked) do
    Enum.each(reqs, fn {name, _app, req, from} ->
      check_package_req(name, req, from)
    end)

    Enum.each(locked, fn {name, _app, req} ->
      check_package_req(name, req, "mix.lock")
    end)
  end

  defp check_package_req(name, req, from) do
    if versions = Registry.get_versions(name) do
      if req != nil and Hex.Version.parse_requirement(req) == :error do
        Mix.raise "Required version #{inspect req} for package #{name} is incorrectly specified (from: #{from})"
      end
      versions = Enum.filter(versions, &Hex.Mix.version_match?(&1, req))
      if versions == [] do
        Mix.raise "No package version in registry matches #{name} #{req} (from: #{from})"
      end
    else
      Mix.raise "No package with name #{name} (from: #{from}) in registry"
    end
  end

  defp print_success(resolved, locked) do
    locked = Enum.map(locked, &elem(&1, 0))
    resolved = Enum.into(resolved, %{}, fn {name, _app, version} -> {name, version} end)
    resolved = Map.drop(resolved, locked)

    if Map.size(resolved) != 0 do
      Hex.Shell.info "Dependency resolution completed"
      resolved = Enum.sort(resolved)
      Enum.each(resolved, fn {name, version} ->
        Hex.Shell.info "  #{name}: #{version}"
      end)
    end
  end

  defp verify_lock(lock) do
    Enum.each(lock, fn
      {_app, {:hex, name, version}} ->
        verify_dep(Atom.to_string(name), version)
      _ ->
        :ok
    end)
  end

  defp verify_dep(app, version) do
    if versions = Registry.get_versions(app) do
      unless version in versions do
        Mix.raise "Unknown package version #{app} #{version} in lockfile"
      end
    else
      Mix.raise "Unknown package #{app} in lockfile"
    end
  end

  defp with_children(apps, lock) do
    [apps, do_with_children(apps, lock)]
    |> List.flatten
  end

  defp do_with_children(names, lock) do
    Enum.map(names, fn name ->
      case Map.fetch(lock, String.to_atom(name)) do
        {:ok, {:hex, name, version}} ->
          # Do not error on bad data in the old lock because we should just
          # fix it automatically
          if deps = Registry.get_deps(Atom.to_string(name), version) do
            apps = Enum.map(deps, &elem(&1, 0))
            [apps, do_with_children(apps, lock)]
          else
            []
          end
        _ ->
          []
      end
    end)
  end

  defp prepare_locked(lock, old_lock, deps) do
    # Remove dependencies from the lock if:
    # 1. They are defined as git or path in mix.exs
    # 2. If the requirement in mix.exs does not match the locked version
    # 3. If it's a child of another Hex package being unlocked/updated

    unlock =
      Enum.filter_map(deps, fn
        %Mix.Dep{scm: Hex.SCM, app: app, requirement: req, opts: opts} ->
          # Make sure to handle deps that were previously locked as Git
          case Map.fetch(old_lock, app) do
            {:ok, {:hex, name, version}} ->
              req && !Hex.Version.match?(version, req) && name == opts[:hex]

            _ ->
              true
          end

        %Mix.Dep{} ->
          true
      end, &Atom.to_string(&1.app))

    unlock =
      for {app, _} <- old_lock,
          not Map.has_key?(lock, app),
        into: unlock,
        do: Atom.to_string(app)

    unlock =
      Enum.uniq(unlock)
      |> with_children(old_lock)
      |> Enum.uniq

    Hex.Mix.from_lock(old_lock)
    |> Enum.reject(fn {_name, app, _vsn} ->
      app in unlock
    end)
  end
end
