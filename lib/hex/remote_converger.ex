defmodule Hex.RemoteConverger do
  @moduledoc false

  alias Hex.Registry

  @behaviour Mix.RemoteConverger

  @registry_updated :registry_updated

  def remote?(dep) do
    !! dep.opts[:hex_app]
  end

  def converge(deps, lock) do
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
      {:ok, {:package, version}} ->

        if Hex.Registry.start() == :ok do
          deps = Registry.get_deps("#{app}", version) || []
          for {app, _, _} <- deps, do: :"#{app}"
        else
          []
        end

      _ ->
        []
    end
  end

  defp check_input(reqs,locked ) do
    Enum.each(reqs, fn {app, req} ->
      check_package_req(app, req, nil)
    end)

    Enum.each(locked, fn {app, req} ->
      check_package_req(app, req, " (from lock)")
    end)
  end

  defp check_package_req(app, req, message) do
    if versions = Registry.get_versions(app) do
      versions = Enum.filter(versions, &Hex.Mix.version_match?(&1, req))
      if versions == [] do
        Mix.raise "No package version in registry matches #{app} #{req}#{message}"
      end
    else
      Mix.raise "No package with name #{app}#{message} in registry"
    end
  end

  defp print_info(reqs, locked, overridden) do
    reqs = Enum.into(reqs, %{})

    unlocked = for {app, _req} <- reqs,
                   not Dict.has_key?(locked, app),
                   do: app

    {overridden, skipping} = Enum.partition(overridden, &Dict.has_key?(reqs, &1))

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
    resolved = Dict.drop(resolved, Dict.keys(locked))
    if resolved != [] do
      Mix.shell.info "Dependency resolution completed successfully"
      Enum.each(resolved, fn {dep, version} ->
        Mix.shell.info "  #{dep}: v#{version}"
      end)
    end
  end

  defp verify_lock(lock) do
    Enum.each(lock, fn
      {app, {:package, version}} ->
        if versions = Registry.get_versions("#{app}") do
          unless version in versions do
            Mix.raise "Unknown package version #{app} v#{version} in lockfile"
          end
        else
          Mix.raise "Unknown package #{app} in lockfile"
        end
      _ ->
        :ok
    end)
  end

  defp with_children(apps, lock) do
    [apps, do_with_children(apps, lock)]
    |> List.flatten
  end

  defp do_with_children(apps, lock) do
    Enum.map(apps, fn app ->
      case Dict.fetch(lock, :"#{app}") do
        {:ok, {:package, version}} ->
          deps = Registry.get_deps(app, version)
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
    # 3. If it's a child of another Hex package

    unlocked =
      Enum.flat_map(deps, fn
        %Mix.Dep{scm: Hex.SCM, app: app, requirement: req} ->
          case Dict.fetch(old_lock, app) do
            {:ok, {:package, vsn}} ->
              if !req or Version.match?(vsn, req) do
                []
              else
                ["#{app}"]
              end
            :error ->
              ["#{app}"]
          end

        %Mix.Dep{app: app} ->
          ["#{app}"]
      end)
        ++
      for({app, _} <- old_lock,
          not Dict.has_key?(lock, app),
          do: "#{app}")

    unlocked = unlocked
               |> Enum.uniq
               |> with_children(old_lock)
               |> Enum.uniq

    for {app, _} = pair <- Hex.Mix.from_lock(old_lock),
        not app in unlocked,
        into: %{}, do: pair
  end
end
