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

    Registry.persist()
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

    overridden = Hex.Mix.overridden_deps(deps)
    requests = Hex.Mix.deps_to_requests(deps, overridden)

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
    verify_deps(deps, Hex.Mix.top_level(deps))
    verify_input(requests, locked)

    Hex.Shell.info("Resolving Hex dependencies...")
    run_solver(lock, old_lock, requests, locked, overridden)
  end

  defp run_solver(lock, old_lock, requests, locked, overridden) do
    start_time = System.monotonic_time(:millisecond)
    dependencies = Enum.map(requests, &request_to_dependency/1)
    locked = Enum.map(locked, &request_to_locked/1)
    overridden = Enum.map(overridden, &Atom.to_string/1)
    verify_otp_app_names(dependencies)

    level = Logger.level()
    Logger.configure(level: if(Hex.State.fetch!(:debug_solver), do: :debug, else: :info))

    solution =
      try do
        Hex.Solver.run(
          Registry,
          dependencies,
          locked,
          overridden,
          ansi: Hex.Shell.ansi_enabled?()
        )
      after
        Logger.configure(level: level)
      end

    current_time = System.monotonic_time(:millisecond)
    total_time = Float.round((current_time - start_time) / 1000, 3)
    Hex.Shell.info("Resolution completed in #{total_time}s")

    case solution do
      {:ok, resolved} ->
        resolved = normalize_resolved(resolved)
        solver_success(resolved, requests, lock, old_lock)

      {:error, message} ->
        Hex.Shell.info(message)
        Mix.raise("Hex dependency resolution failed")
    end
  end

  defp request_to_dependency(request) do
    constraint =
      if request.requirement do
        Hex.Solver.parse_constraint!(request.requirement)
      else
        %Hex.Solver.Constraints.Range{}
      end

    %{
      repo: if(request.repo != "hexpm", do: request.repo),
      name: request.name,
      constraint: constraint,
      optional: false,
      label: request.app,
      from: request.from,
      dependencies: Enum.map(request.dependencies, &request_to_dependency/1)
    }
  end

  defp request_to_locked(request) do
    %{
      repo: if(request.repo != "hexpm", do: request.repo),
      name: request.name,
      version: Hex.Solver.parse_constraint!(request.version),
      label: request.app
    }
  end

  def normalize_resolved(resolved) do
    resolved
    |> Enum.map(fn {name, {version, repo}} ->
      {repo || "hexpm", name, to_string(version)}
    end)
    |> Enum.sort()
  end

  defp solver_success(resolved, requests, lock, old_lock) do
    parent_deps =
      Enum.flat_map(requests, fn %{name: package, dependencies: deps} ->
        if deps == [] do
          []
        else
          [package]
        end
      end)

    resolved = Enum.reject(resolved, fn {_repo, package, _version} -> package in parent_deps end)
    resolved = add_apps_to_resolved(resolved, requests)
    print_success(resolved, old_lock)
    verify_resolved(resolved, old_lock)
    new_lock = Hex.Mix.to_lock(resolved)
    Hex.SCM.prefetch(new_lock)
    lock_merge(lock, new_lock)
  end

  defp add_apps_to_resolved(resolved, requests) do
    resolved =
      Enum.map(resolved, fn {repo, package, version} ->
        apps =
          Enum.flat_map(resolved, fn {parent_repo, parent_package, parent_version} ->
            repo = if repo != "hexpm", do: repo

            deps =
              case Registry.dependencies(parent_repo, parent_package, parent_version) do
                {:ok, deps} -> deps
                :error -> []
              end

            app =
              Enum.find_value(deps, fn
                %{repo: ^repo, name: ^package, label: app} -> app
                %{} -> nil
              end)

            if app do
              [{parent_repo, parent_package, app}]
            else
              []
            end
          end)

        root_app = find_root_app_in_requests(requests, repo, package)
        apps = Enum.uniq_by(List.wrap(root_app) ++ apps, fn {_repo, _package, app} -> app end)

        case apps do
          [{_repo, _package, app}] ->
            {repo, package, app, version}

          apps when apps != [] ->
            name = Hex.Utils.package_name(repo, package)

            header =
              "Conflicting OTP application names in dependency definition of " <>
                "\"#{name} {version}\", in the following packages:\n\n"

            list =
              Enum.map_join(apps, "\n", fn {parent_repo, parent_package, app} ->
                name = Hex.Utils.package_name(parent_repo, parent_package)
                "  * #{name} defined application :#{app}"
              end)

            Mix.raise(header <> list)
        end
      end)

    Enum.each(resolved, fn {_repo, _package, app, _version} ->
      conflicting =
        Enum.filter(resolved, fn
          {_repo, _package, ^app, _version} -> true
          {_repo, _package, _app, _version} -> false
        end)

      unless length(conflicting) == 1 do
        header =
          "Multiple packages resolved with the same OTP application name of \"#{app}\":\n\n"

        list =
          Enum.map_join(conflicting, "\n", fn {repo, package, _app, version} ->
            name = Hex.Utils.package_name(repo, package)
            "  * #{name} #{version}"
          end)

        Mix.raise(header <> list)
      end
    end)

    resolved
  end

  defp find_root_app_in_requests(requests, repo, package) do
    Enum.find_value(requests, fn
      %{repo: ^repo, name: ^package, app: app} -> {"hexpm", "myapp", app}
      %{dependencies: dependencies} -> find_root_app_in_requests(dependencies, repo, package)
      %{} -> nil
    end)
  end

  defp packages_from_requests(deps) do
    Enum.flat_map(deps, fn
      %{repo: repo, name: package, dependencies: []} -> [{repo, package}]
      %{dependencies: dependencies} -> packages_from_requests(dependencies)
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
    deps =
      case Registry.dependencies(repo, name, version) do
        {:ok, deps} -> deps
        :error -> []
      end

    for %{repo: repo, name: name, constraint: req, optional: optional, label: app} <- deps do
      app = String.to_atom(app)
      opts = [optional: optional, hex: name, repo: repo]
      {app, to_string(req), opts}
    end
  end

  # TODO: Use requests
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
    Enum.each(requests, fn
      %{repo: repo, name: name, requirement: req, from: from, dependencies: []} ->
        verify_package_req(repo, name, req, from)

      %{} ->
        nil
    end)

    Enum.each(locked, fn %{repo: repo, name: name, version: req} ->
      verify_package_req(repo, name, req, "mix.lock")
    end)
  end

  defp verify_package_req(repo, name, req, from) do
    if Registry.versions(repo, name) == :error do
      Mix.raise("No package with name #{name} (from: #{from}) in registry")
    end

    if req != nil and Version.parse_requirement(req) == :error do
      Mix.raise(
        "Required version #{inspect(req)} for package #{name} is incorrectly specified (from: #{from})"
      )
    end
  end

  defp verify_otp_app_names(dependencies) do
    verify_otp_app_names(dependencies, %{})
  end

  defp verify_otp_app_names([%{dependencies: []} = dependency | dependencies], map) do
    map =
      Map.update(map, {dependency.repo, dependency.name}, dependency, fn existing_dependency ->
        if existing_dependency.label != dependency.label do
          name = Hex.Utils.package_name(dependency.repo, dependency.name)

          Mix.raise("""
          Conflicting OTP application names in dependency definition of #{inspect(name)}, in the following locations:

            * #{existing_dependency.from} defined application :#{existing_dependency.label}
            * #{dependency.from} defined application :#{dependency.label}
          """)
        end
      end)

    verify_otp_app_names(dependencies, map)
  end

  defp verify_otp_app_names([%{dependencies: sub_dependencies} | dependencies], map) do
    verify_otp_app_names(sub_dependencies ++ dependencies, map)
  end

  defp verify_otp_app_names([], _map) do
    :ok
  end

  defp print_success(resolved, old_lock) do
    if resolved != [] do
      previously_locked_versions = dep_info_from_lock(old_lock)
      dep_changes = group_dependency_changes(resolved, previously_locked_versions)

      Enum.each(dep_changes, fn {mod, deps} ->
        unless length(deps) == 0, do: print_category(mod)
        print_dependency_group(deps, mod)
      end)
    end
  end

  defp group_dependency_changes(resolved, previously_locked_versions) do
    state = %{new: [], eq: [], gt: [], lt: []}

    resolved
    |> Enum.map(fn {repo, name, _app, version} -> {name, {repo, version}} end)
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
    |> Map.new()
  end

  defp version_string_or_nil(nil), do: nil
  defp version_string_or_nil({_repo, version_string}), do: version_string

  defp categorize_dependency_change(nil, _version), do: :new

  defp categorize_dependency_change(previous_version, version) do
    Version.compare(version, previous_version)
  end

  defp warning_message(nil, _version), do: nil

  defp warning_message(previous_version, version) do
    prev_ver = Version.parse!(previous_version)
    new_ver = Version.parse!(version)

    cond do
      major_version_change?(prev_ver, new_ver) -> " (major)"
      breaking_minor_version_change?(prev_ver, new_ver) -> " (minor)"
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
    # TODO: Use requests?
    deps =
      Enum.map(deps, fn {app, req, opts} ->
        %{
          repo: opts[:repo],
          name: opts[:hex],
          constraint: Hex.Solver.parse_constraint!(req),
          optional: !!opts[:optional],
          label: Atom.to_string(app)
        }
      end)

    registry_deps =
      case Registry.dependencies(repo, name, version) do
        {:ok, deps} -> deps
        :error -> []
      end

    if Enum.sort(deps) != Enum.sort(registry_deps) do
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

    case Registry.versions(repo, name) do
      {:ok, versions} ->
        versions = Enum.map(versions, &to_string/1)

        unless version in versions do
          Mix.raise("Unknown package version #{name} #{version} in lockfile")
        end

      :error ->
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
          case Registry.dependencies(repo, name, version) do
            {:ok, deps} ->
              apps = Enum.map(deps, fn %{label: app} -> app end)
              [apps, do_with_children(apps, lock)]

            :error ->
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
    |> Enum.reject(fn %{app: app} -> app in unlock end)
  end

  defp unlock_deps(deps, old_lock) do
    Enum.filter(deps, fn
      %Mix.Dep{scm: Hex.SCM, app: app, requirement: req, opts: opts} ->
        name = opts[:hex]

        case Hex.Utils.lock(old_lock[app]) do
          %{name: ^name, version: version, repo: repo} ->
            (req && not Version.match?(version, req)) || repo != opts[:repo]

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

  defp major_version_change?(%Version{} = version1, %Version{} = version2) do
    version1.major != version2.major
  end

  defp breaking_minor_version_change?(%Version{} = version1, %Version{} = version2) do
    version1.major == 0 and version2.major == 0 and version1.minor != version2.minor
  end
end
