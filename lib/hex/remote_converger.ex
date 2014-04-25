defmodule Hex.RemoteConverger do
  @moduledoc false

  alias Hex.Registry

  @behaviour Mix.RemoteConverger

  @registry_updated :registry_updated

  def remote?(dep) do
    !! dep.opts[:hex_app]
  end

  def converge(deps, lock) do
    Hex.Util.ensure_registry()

    # We cannot use given lock here, because all deps that are being
    # converged have been removed from the lock by Mix
    # We need the old lock to get the children of Hex packages

    old_lock = Mix.Dep.Lock.read
    verify_lock(lock)

    # Make sure to unlock all children of Hex packages
    unlocked =
      for { app, _ } <- old_lock,
          not Dict.has_key?(lock, app),
          do: "#{app}"
    unlocked = with_children(unlocked, old_lock)

    locked = for { app, _ } = pair <- Hex.Mix.from_lock(old_lock),
                 not app in unlocked,
                 into: %{}, do: pair

    reqs = Hex.Mix.deps_to_requests(deps)
    overridden = Hex.Mix.deps_to_overridden(deps)

    print_info(reqs, locked, overridden)

    if resolved = Hex.Resolver.resolve(reqs, overridden, locked) do
      print_success(resolved, locked)
      new_lock = Hex.Mix.to_lock(resolved)
      Dict.merge(lock, new_lock)
    else
      raise Mix.Error, message: "Hex dependency resolution failed, relax the version requirements or unlock dependencies"
    end
  end

  def deps(%Mix.Dep{app: app}, lock) do
    case Dict.fetch(lock, app) do
      {:ok, {:package, version}} ->
        Hex.Registry.start

        scms = Mix.SCM.available
        {_, deps} = Registry.get_release("#{app}", version)

        Enum.map(deps, fn {app, _} ->
          Mix.Dep.Loader.to_dep({:"#{app}", []}, scms, "Hex")
        end)

      :error ->
        []
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
      Enum.each(resolved, fn { dep, version } ->
        Mix.shell.info "  #{dep}: v#{version}"
      end)
    end
  end

  defp verify_lock(lock) do
    Enum.each(lock, fn
      { app, { :package, version } } ->
        if versions = Registry.get_versions("#{app}") do
          unless version in versions do
            raise Mix.Error, message: "Unknown package version #{app} v#{version} in lockfile"
          end
        else
          raise Mix.Error, message: "Unknown package #{app} in lockfile"
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
        { :ok, { :package, version } } ->
          { _, deps } = Registry.get_release(app, version)
          deps = Enum.map(deps, &elem(&1, 0))
          [deps, do_with_children(deps, lock)]
        _ ->
          []
      end
    end)
  end
end
