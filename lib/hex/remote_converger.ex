defmodule Hex.RemoteConverger do
  @moduledoc false

  alias Hex.Registry

  @behaviour Mix.RemoteConverger

  @registry_updated :registry_updated

  def remote?(dep) do
    !! dep.opts[:hex]
  end

  def converge(deps, lock) do
    Hex.start
    Hex.Util.ensure_registry!()

    verify_lock(lock)

    # We cannot use given lock here, because all deps that are being
    # converged have been removed from the lock by Mix
    # We need the old lock to get the children of Hex packages
    old_lock = Mix.Dep.Lock.read

    reqs       = Hex.Mix.deps_to_requests(deps)
    overridden = Hex.Mix.deps_to_overridden(deps)
    locked     = prepare_locked(lock, old_lock, deps)

    check_input(reqs, locked)
    print_info(reqs, locked, overridden)

    if resolved = Hex.Resolver.resolve(reqs, overridden, locked) do
      print_success(resolved, locked)
      new_lock = Hex.Mix.to_lock(resolved)
      Hex.SCM.prefetch(new_lock)

      Dict.merge(lock, new_lock)
    else
      Mix.raise "Hex dependency resolution failed, relax the version requirements or unlock dependencies"
    end
  end

  def deps(%Mix.Dep{app: app}, lock) do
    case Dict.fetch(lock, app) do
      # Support older
      {:ok, {:package, version}} ->
        get_deps(app, version)
      {:ok, {:hex, name, version}} ->
        get_deps(name, version)
      _ ->
        []
    end
  end

  defp get_deps(name, version) do
    if Hex.Registry.start() == :ok do
      name = Atom.to_string(name)
      deps = Registry.get_deps(name, version) || []
      for {name, app, req, optional} <- deps do
        {String.to_atom(app), req, optional: optional, hex: String.to_atom(name)}
      end
    else
      []
    end
  end

  defp check_input(reqs, locked) do
    Enum.each(reqs, fn {name, _app, req} ->
      check_package_req(name, req, nil)
    end)

    Enum.each(locked, fn {name, _app, req} ->
      check_package_req(name, req, " (from lock)")
    end)
  end

  defp check_package_req(name, req, message) do
    if versions = Registry.get_versions(name) do
      versions = Enum.filter(versions, &Hex.Mix.version_match?(&1, req))
      if versions == [] do
        Mix.raise "No package version in registry matches #{name} #{req}#{message}"
      end
    else
      Mix.raise "No package with name #{name}#{message} in registry"
    end
  end

  defp print_info(reqs, locked, overridden) do
    reqs   = Enum.into(reqs, HashSet.new, &elem(&1, 0))
    locked = Enum.into(locked, HashSet.new, &elem(&1, 0))

    unlocked = Enum.reject(reqs, &HashSet.member?(locked, &1))

    {overridden, skipping} = Enum.partition(overridden, &HashSet.member?(reqs, &1))

    if unlocked != [] do
      Mix.shell.info "Running dependency resolution"
      Mix.shell.info "Unlocked:   " <> Enum.join(unlocked, ", ")

      if overridden != [],
        do: Mix.shell.info "Overridden: " <> Enum.join(overridden, ", ")
      if skipping != [],
        do: Mix.shell.info "Skipping:   " <> Enum.join(skipping, ", ")
    end
  end

  defp print_success(resolved, locked) do
    locked = Enum.map(locked, &elem(&1, 0))
    resolved = Enum.into(resolved, HashDict.new, fn {name, _app, version} -> {name, version} end)
    resolved = HashDict.drop(resolved, locked)

    if resolved != [] do
      Mix.shell.info "Dependency resolution completed successfully"
      Enum.each(resolved, fn {name, version} ->
        Mix.shell.info "  #{name}: v#{version}"
      end)
    end
  end

  defp verify_lock(lock) do
    Enum.each(lock, fn
      # Support older
      {app, {:package, version}} ->
        verify_dep(Atom.to_string(app), version)
      {_app, {:package, name, version}} ->
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
      case Dict.fetch(lock, String.to_atom(name)) do
        # Support older
        {:ok, {:package, version}} ->
          deps = Registry.get_deps(name, version)
                 |> Enum.map(&elem(&1, 0))
          [deps, do_with_children(deps, lock)]
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
          case Dict.fetch(old_lock, app) do
            # Support older
            {:ok, {:package, version}} ->
              req && !Version.match?(version, req)

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
          not Dict.has_key?(lock, app),
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
