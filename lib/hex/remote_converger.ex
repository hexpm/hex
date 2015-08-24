defmodule Hex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  alias Hex.Registry

  def remote?(dep) do
    !! dep.opts[:hex]
  end

  def converge(deps, lock) do
    Hex.start
    Hex.Utils.ensure_registry!()

    verify_lock(lock)

    # We cannot use given lock here, because all deps that are being
    # converged have been removed from the lock by Mix
    # We need the old lock to get the children of Hex packages
    old_lock = Mix.Dep.Lock.read

    locked     = prepare_locked(lock, old_lock, deps)
    flat_deps  = Hex.Mix.flatten_deps(deps)
    reqs       = Hex.Mix.deps_to_requests(flat_deps)

    check_input(reqs, locked)

    Hex.Shell.info "Running dependency resolution"

    case Hex.Resolver.resolve(reqs, deps, locked) do
      {:ok, resolved} ->
        print_success(resolved, locked)
        new_lock = Hex.Mix.to_lock(resolved)
        Hex.SCM.prefetch(new_lock)
        Map.merge(lock, new_lock)

      {:error, messages} ->
        Hex.Shell.error messages
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
    if Hex.Registry.open() == :ok do
      name = Atom.to_string(name)
      deps = Registry.get_deps(name, version) || []
      for {name, app, req, optional} <- deps do
        {String.to_atom(app), req, optional: optional, hex: String.to_atom(name)}
      end
    else
      if File.exists?(Hex.Registry.path),
          do: Hex.Shell.warn("Missing Hex registry file, run `mix hex.info` to fetch"),
        else: Hex.Shell.warn("Failed to open Hex registry file")
      []
    end
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
    resolved = Enum.into(resolved, HashDict.new, fn {name, _app, version} -> {name, version} end)
    resolved = HashDict.drop(resolved, locked)

    if HashDict.size(resolved) != 0 do
      Hex.Shell.info "Dependency resolution completed successfully"
      resolved = Enum.sort(resolved)
      Enum.each(resolved, fn {name, version} ->
        Hex.Shell.info "  #{name}: v#{version}"
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
        Mix.raise "Unknown package version #{app} v#{version} in lockfile"
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
          deps = Registry.get_deps(Atom.to_string(name), version)
                 |> Enum.map(&elem(&1, 0))
          [deps, do_with_children(deps, lock)]
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
      Enum.filter(deps, fn
        %Mix.Dep{scm: Hex.SCM, app: app, requirement: req, opts: opts} ->
          # Make sure to handle deps that were previously locked as Git
          case Map.fetch(old_lock, app) do
            {:ok, {:hex, name, version}} ->
              req && !Version.match?(version, req) && name == opts[:hex]

            _ ->
              true
          end

        %Mix.Dep{} ->
          true
      end)
      |> Enum.map(&Atom.to_string(&1.app))

    unlock = unlock ++
      for({app, _} <- old_lock,
          not Map.has_key?(lock, app),
          do: Atom.to_string(app))

    unlock = unlock
             |> Enum.uniq
             |> with_children(old_lock)
             |> Enum.uniq

    Enum.reject(Hex.Mix.from_lock(old_lock), fn {_name, app, _vsn} ->
      app in unlock
    end)
  end
end
