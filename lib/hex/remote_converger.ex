defmodule Hex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  alias Hex.Registry.Server, as: Registry

  def post_converge() do
    Hex.UpdateChecker.check()

    if Hex.State.get(:print_sponsored_tip) do
      Hex.Shell.info(
        "You have added/upgraded packages you could sponsor, " <>
          "run `mix hex.sponsor` to learn more"
      )

      Hex.State.put(:print_sponsored_tip, false)
    end

    Registry.close()
  end

  def remote?(dep) do
    !!dep.opts[:hex]
  end

  def converge(deps, lock) do
    Registry.open()

    # We cannot use given lock here, because all deps that are being
    # converged have been removed from the lock by Mix
    # We need the old lock to get the children of Hex packages
    old_lock = Mix.Dep.Lock.read()

    top_level = Hex.Mix.top_level(deps)
    flat_deps = Hex.Mix.flatten_deps(deps, top_level)
    requests = Hex.Mix.deps_to_requests(flat_deps)

    [
      Hex.Mix.packages_from_lock(lock),
      Hex.Mix.packages_from_lock(old_lock),
      packages_from_requests(requests)
    ]
    |> Enum.concat()
    |> verify_prefetches()
    |> Registry.prefetch()

    locked = prepare_locked(lock, old_lock, deps)

    verify_lock(lock)
    verify_deps(deps, top_level)
    verify_input(requests, locked)

    repos = repo_overrides(deps)
    deps = Hex.Mix.prepare_deps(deps)

    top_level = Enum.map(top_level, &Atom.to_string/1)

    Hex.Shell.info("Resolving Hex dependencies...")

    case Hex.Resolver.resolve(Registry, requests, deps, top_level, repos, locked) do
      {:ok, resolved} ->
        print_success(resolved, locked, old_lock)
        verify_resolved(resolved, old_lock)
        new_lock = Hex.Mix.to_lock(resolved)
        Hex.SCM.prefetch(new_lock)
        lock_merge(lock, new_lock)

      {:error, {:version, message}} ->
        resolver_version_failed(message)

      {:error, {:repo, message}} ->
        resolver_repo_failed(message)
    end
  after
    if Version.compare(System.version(), "1.4.0") == :lt do
      Registry.persist()
    end
  end

  defp repo_overrides(deps) do
    for dep <- deps,
        dep.top_level,
        into: %{},
        do: {dep.opts[:hex], dep.opts[:repo]}
  end

  defp packages_from_requests(deps) do
    Enum.map(deps, fn {repo, package, _app, _req, _from} ->
      {repo, package}
    end)
  end

  defp lock_merge(old, new) do
    Map.merge(old, new, fn _key, old_tuple, new_tuple ->
      if lock_tuple_needs_update?(old_tuple, new_tuple) do
        new_tuple
      else
        old_tuple
      end
    end)
  end

  defp lock_tuple_needs_update?(old_tuple, new_tuple) do
    old_info = Hex.Utils.lock(old_tuple)
    new_info = Hex.Utils.lock(new_tuple)

    not (old_info != nil and
           new_info != nil and
           old_info.name == new_info.name and
           old_info.version == new_info.version and
           old_info.inner_checksum == new_info.inner_checksum and
           old_info.outer_checksum == new_info.outer_checksum and
           old_info.repo == new_info.repo)
  end

  defp resolver_version_failed(message) do
    Hex.Shell.info("\n" <> message)

    Mix.raise(
      "Hex dependency resolution failed, change the version " <>
        "requirements of your dependencies or unlock them (by " <>
        "using mix deps.update or mix deps.unlock). If you are " <>
        "unable to resolve the conflicts you can try overriding " <>
        "with {:dependency, \"~> 1.0\", override: true}"
    )
  end

  defp resolver_repo_failed(message) do
    Hex.Shell.info("\n" <> message)

    Mix.raise(
      "Hex dependency resolution failed because of repo conflicts. " <>
        "You can override the repo by adding it as a dependency " <>
        "{:dependency, \"~> 1.0\", repo: \"my_repo\"}"
    )
  end

  def deps(%Mix.Dep{app: app}, lock) do
    case Hex.Utils.lock(lock[app]) do
      %{name: name, version: version, deps: nil, repo: repo} ->
        Registry.open()
        Registry.prefetch([{repo, name}])
        get_deps(repo, name, version)

      %{deps: deps} ->
        deps

      nil ->
        []
    end
  end

  defp get_deps(repo, name, version) do
    deps = Registry.deps(repo, name, version) || []

    for {repo, name, app, req, optional} <- deps do
      app = String.to_atom(app)
      opts = [optional: optional, hex: name, repo: repo]

      # Support old packages where requirement could be missing
      if req do
        {app, req, opts}
      else
        {app, opts}
      end
    end
  end

  defp verify_deps(deps, top_level) do
    Enum.each(deps, fn dep ->
      if dep.app in top_level and dep.scm == Hex.SCM and dep.requirement == nil do
        Hex.Shell.warn(
          "#{dep.app} is missing its version requirement, " <>
            "use \">= 0.0.0\" if it should match any version"
        )
      end
    end)
  end

  defp verify_input(requests, locked) do
    Enum.each(requests, fn {repo, name, _app, req, from} ->
      verify_package_req(repo, name, req, from)
    end)

    Enum.each(locked, fn {repo, name, _app, req} ->
      verify_package_req(repo, name, req, "mix.lock")
    end)
  end

  defp verify_package_req(repo, name, req, from) do
    versions = Registry.versions(repo, name)

    unless versions do
      Mix.raise("No package with name #{name} (from: #{from}) in registry")
    end

    if req != nil and Hex.Version.parse_requirement(req) == :error do
      Mix.raise(
        "Required version #{inspect(req)} for package #{name} is incorrectly specified (from: #{
          from
        })"
      )
    end

    if req != nil and not Enum.any?(versions, &Hex.Version.match?(&1, req)) do
      Mix.raise(
        "No matching version for #{name} #{req} (from: #{from}) in registry#{
          matching_versions_message(versions, req)
        }"
      )
    end
  end

  defp matching_versions_message(versions, req) do
    versions = Enum.map(versions, &Hex.Version.parse!/1)
    pre_versions = matching_pre_versions(versions, req)

    if pre_versions != [] do
      "\n\nWhile there is no package matching the requirement above, there are pre-releases available:\n\n" <>
        Enum.map_join(pre_versions, "\n", &"  * #{&1}") <>
        "\n\nIn order to match any of them, you need to include the \"-pre\" suffix in your requirement, " <>
        "where \"pre\" is one of the suffixes listed above."
    else
      latest = List.last(versions)
      "\n\nThe latest version is: #{latest}"
    end
  end

  defp matching_pre_versions(versions, req) do
    for version <- versions, version.pre != [], Hex.Version.match?(%{version | pre: []}, req) do
      version
    end
  end

  defp print_success(resolved, locked, old_lock) do
    previously_locked_versions = dep_info_from_lock(old_lock)
    resolved = resolve_dependencies(resolved, locked)

    if map_size(resolved) != 0 do
      Hex.Shell.info("Dependency resolution completed:")

      dep_changes = group_dependency_changes(resolved, previously_locked_versions)

      Enum.each(dep_changes, fn {mod, deps} ->
        unless length(deps) == 0, do: print_category(mod)
        print_dependency_group(deps, mod)
      end)
    end
  end

  defp resolve_dependencies(resolved, locked) do
    locked = Enum.map(locked, &elem(&1, 0))

    resolved
    |> Enum.into(%{}, fn {repo, name, _app, version} -> {name, {repo, version}} end)
    |> Map.drop(locked)
  end

  defp group_dependency_changes(resolved, previously_locked_versions) do
    state = %{new: [], eq: [], gt: [], lt: []}

    resolved
    |> Enum.sort()
    |> Enum.reduce(state, fn {name, {repo, version}}, acc ->
      previous_version =
        previously_locked_versions
        |> Map.get(name)
        |> version_string_or_nil()

      change = categorize_dependency_change(previous_version, version)
      warning = warning_message(previous_version, version)
      Map.put(acc, change, acc[change] ++ [{name, repo, previous_version, version, warning}])
    end)
  end

  defp dep_info_from_lock(lock) do
    Enum.flat_map(lock, fn {_app, info} ->
      case Hex.Utils.lock(info) do
        %{name: name, repo: repo, version: version} ->
          [{name, {repo, version}}]

        nil ->
          []
      end
    end)
    |> Enum.into(%{})
  end

  defp version_string_or_nil(nil), do: nil
  defp version_string_or_nil({_repo, version_string}), do: version_string

  defp categorize_dependency_change(nil, _version), do: :new

  defp categorize_dependency_change(previous_version, version) do
    Version.compare(version, previous_version)
  end

  defp warning_message(nil, _version), do: nil

  defp warning_message(previous_version, version) do
    prev_ver = Hex.Version.parse!(previous_version)
    new_ver = Hex.Version.parse!(version)

    cond do
      Hex.Version.major_version_change?(prev_ver, new_ver) -> " (major)"
      Hex.Version.breaking_minor_version_change?(prev_ver, new_ver) -> " (minor)"
      true -> nil
    end
  end

  defp print_category(mod) do
    case mod do
      :new -> Hex.Shell.info("New:")
      :eq -> Hex.Shell.info("Unchanged:")
      :lt -> Hex.Shell.info("Downgraded:")
      :gt -> Hex.Shell.info("Upgraded:")
    end
  end

  defp print_dependency_group(deps, mod) do
    Enum.each(deps, fn {name, repo, previous_version, version, warning} ->
      print_status(
        Registry.retired(repo, name, version),
        mod,
        name,
        previous_version,
        version,
        warning
      )
    end)
  end

  defp print_status(nil, mod, name, previous_version, version, warning) do
    case mod do
      :new ->
        Hex.Shell.info(Hex.Shell.format([:green, "  #{name} #{version}", :red, "#{warning}"]))

      :eq ->
        Hex.Shell.info("  #{name} #{version}")

      :lt ->
        Hex.Shell.info(
          Hex.Shell.format([
            :yellow,
            "  #{name} #{previous_version} => #{version}",
            :red,
            "#{warning}"
          ])
        )

      :gt ->
        Hex.Shell.info(
          Hex.Shell.format([
            :green,
            "  #{name} #{previous_version} => #{version}",
            :red,
            "#{warning}"
          ])
        )
    end
  end

  defp print_status(retired, mod, name, previous_version, version, _warning) do
    case mod do
      mod when mod in [:eq, :new] ->
        Hex.Shell.warn("  #{name} #{version} RETIRED!")
        Hex.Shell.warn("    #{Hex.Utils.package_retirement_message(retired)}")

      _ ->
        Hex.Shell.warn("  #{name} #{previous_version} => #{version} RETIRED!")
        Hex.Shell.warn("    #{Hex.Utils.package_retirement_message(retired)}")
    end
  end

  defp verify_prefetches(prefetches) do
    prefetches
    |> Enum.map(fn {repo, _package} -> repo end)
    |> Enum.uniq()
    |> Enum.each(&verify_repo/1)

    prefetches
  end

  defp verify_repo(repo) do
    case Hex.Repo.fetch_repo(repo) do
      {:ok, _} ->
        :ok

      :error ->
        case repo do
          "hexpm:" <> organization ->
            if Hex.Shell.yes?(
                 "No authenticated organization found for #{organization}. Do you want to authenticate it now?"
               ) do
              Mix.Tasks.Hex.Organization.run(["auth", organization])
            else
              Hex.Repo.get_repo(repo)
            end

          _ ->
            Mix.raise(
              "Unknown repository #{inspect(repo)}, add new repositories " <>
                "with the `mix hex.repo add` task"
            )
        end
    end
  end

  defp verify_resolved(resolved, lock) do
    Enum.each(resolved, fn {repo, name, app, version} ->
      atom_name = String.to_atom(name)

      case Hex.Utils.lock(lock[String.to_atom(app)]) do
        %{name: ^atom_name, version: ^version, repo: ^repo} = lock ->
          verify_inner_checksum(repo, name, version, lock.inner_checksum)
          verify_outer_checksum(repo, name, version, lock.outer_checksum)
          verify_deps(repo, name, version, lock.deps)

        _ ->
          :ok
      end
    end)
  end

  defp verify_inner_checksum(repo, name, version, checksum) do
    registry_checksum = Registry.inner_checksum(repo, name, version)

    if checksum && Base.decode16!(checksum, case: :lower) != registry_checksum do
      Mix.raise("Registry checksum mismatch against lock (#{name} #{version})")
    end
  end

  defp verify_outer_checksum(repo, name, version, checksum) do
    registry_checksum = Registry.outer_checksum(repo, name, version)

    if checksum && Base.decode16!(checksum, case: :lower) != registry_checksum do
      Mix.raise("Registry checksum mismatch against lock (#{name} #{version})")
    end
  end

  defp verify_deps(nil, _name, _version, _deps), do: :ok

  defp verify_deps(repo, name, version, deps) do
    deps =
      Enum.map(deps, fn {app, req, opts} ->
        {
          opts[:repo],
          opts[:hex],
          Atom.to_string(app),
          req,
          !!opts[:optional]
        }
      end)

    if Enum.sort(deps) != Enum.sort(Registry.deps(repo, name, version)) do
      Mix.raise("Registry dependencies mismatch against lock (#{name} #{version})")
    end
  end

  defp verify_lock(lock) do
    Enum.each(lock, fn {_app, info} ->
      case Hex.Utils.lock(info) do
        %{name: name, version: version, repo: repo} ->
          verify_dep(repo, name, version)

        nil ->
          :ok
      end
    end)
  end

  defp verify_dep(repo, name, version) do
    Hex.Repo.get_repo(repo)

    if versions = Registry.versions(repo, name) do
      unless version in versions do
        Mix.raise("Unknown package version #{name} #{version} in lockfile")
      end
    else
      Mix.raise("Unknown package #{name} in lockfile")
    end
  end

  defp with_children(apps, lock) do
    [apps, do_with_children(apps, lock)]
    |> List.flatten()
  end

  defp do_with_children(names, lock) do
    Enum.map(names, fn name ->
      case Hex.Utils.lock(lock[String.to_atom(name)]) do
        %{name: name, version: version, deps: nil, repo: repo} ->
          # Do not error on bad data in the old lock because we should just
          # fix it automatically
          if deps = Registry.deps(repo, name, version) do
            apps = Enum.map(deps, &elem(&1, 1))
            [apps, do_with_children(apps, lock)]
          else
            []
          end

        %{deps: deps} ->
          apps = Enum.map(deps, &Atom.to_string(elem(&1, 0)))
          [apps, do_with_children(apps, lock)]

        nil ->
          []
      end
    end)
  end

  defp prepare_locked(lock, old_lock, deps) do
    # Remove dependencies from the lock if:
    # 1. They are defined as git or path in mix.exs
    # 2. If the requirement or repo in mix.exs does not match the locked version
    # 3. If it's a child of another Hex package being unlocked/updated

    unlock_without_children =
      for {app, _} <- old_lock,
          not Map.has_key?(lock, app),
          do: Atom.to_string(app)

    unlock =
      (unlock_deps(deps, old_lock) ++ unlock_without_children)
      |> Enum.uniq()
      |> with_children(old_lock)
      |> Enum.uniq()

    old_lock
    |> Hex.Mix.from_lock()
    |> Enum.reject(fn {_repo, _name, app, _version} -> app in unlock end)
  end

  defp unlock_deps(deps, old_lock) do
    Enum.filter(deps, fn
      %Mix.Dep{scm: Hex.SCM, app: app, requirement: req, opts: opts} ->
        name = opts[:hex]

        case Hex.Utils.lock(old_lock[app]) do
          %{name: ^name, version: version, repo: repo} ->
            (req && !Hex.Version.match?(version, req)) || repo != opts[:repo]

          %{} ->
            false

          nil ->
            true
        end

      %Mix.Dep{} ->
        true
    end)
    |> Enum.map(&Atom.to_string(&1.app))
  end
end
