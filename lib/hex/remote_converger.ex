defmodule Hex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  alias Hex.Registry.Server, as: Registry

  def post_converge do
    Registry.close()
    :ok
  end

  def remote?(dep) do
    !!dep.opts[:hex]
  end

  def converge(deps, lock) do
    Hex.start
    Registry.open()

    # We cannot use given lock here, because all deps that are being
    # converged have been removed from the lock by Mix
    # We need the old lock to get the children of Hex packages
    old_lock = Mix.Dep.Lock.read

    top_level = Hex.Mix.top_level(deps)
    flat_deps = Hex.Mix.flatten_deps(deps, top_level)
    requests  = Hex.Mix.deps_to_requests(flat_deps)

    [Hex.Mix.packages_from_lock(lock),
     Hex.Mix.packages_from_lock(old_lock),
     packages_from_requests(requests)]
    |> Enum.concat
    |> Registry.prefetch

    locked = prepare_locked(lock, old_lock, deps)

    check_lock(lock)
    check_deps(deps, top_level)
    check_input(requests, locked)

    repos = repo_overrides(deps)
    deps = Hex.Mix.prepare_deps(deps)
    top_level = Enum.map(top_level, &Atom.to_string/1)

    Hex.Shell.info "Running dependency resolution..."

    case Hex.Resolver.resolve(Registry, requests, deps, top_level, repos, locked) do
      {:ok, resolved} ->
        print_success(resolved, locked)
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
    if Version.compare(System.version, "1.4.0") == :lt do
      Registry.persist
    end
  end

  defp repo_overrides(deps) do
    for dep <- deps, dep.top_level, into: %{},
        # do: {dep.opts[:hex], dep.opts[:original_repo]}
        do: {dep.opts[:hex], dep.opts[:repo]}
  end

  defp packages_from_requests(deps) do
    Enum.map(deps, fn {repo, package, _app, _req, _from} ->
      {repo, package}
    end)
  end

  defp lock_merge(old, new) do
    Map.merge(old, new, fn(_key, old_tuple, new_tuple) ->
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
    not(old_info != nil and
        new_info != nil and
        old_info.name == new_info.name and
        old_info.version == new_info.version and
        old_info.checksum == new_info.checksum and
        old_info.repo == new_info.repo)
  end

  defp resolver_version_failed(message) do
    Hex.Shell.info "\n" <> message

    Mix.raise "Hex dependency resolution failed, relax the version " <>
              "requirements of your dependencies or unlock them (by " <>
              "using mix deps.update or mix deps.unlock). If you are " <>
              "unable to resolve the conflicts you can try overriding " <>
              "with {:dependency, \"~> 1.0\", override: true}"
  end

  defp resolver_repo_failed(message) do
    Hex.Shell.info "\n" <> message

    Mix.raise "Hex dependency resolution failed because of repo conflicts. " <>
              "You can override the repo by adding it as a dependency " <>
              "{:dependency, \"~> 1.0\", repo: \"my_repo\"}"
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

  defp check_deps(deps, top_level) do
    Enum.each(deps, fn dep ->
      if dep.app in top_level and dep.scm == Hex.SCM and is_nil(dep.requirement) do
        Hex.Shell.warn "#{dep.app} is missing its version requirement, " <>
                       "use \">= 0.0.0\" if it should match any version"
      end
    end)
  end

  defp check_input(requests, locked) do
    Enum.each(requests, fn {repo, name, _app, req, from} ->
      check_package_req(repo, name, req, from)
    end)

    Enum.each(locked, fn {repo, name, _app, req} ->
      check_package_req(repo, name, req, "mix.lock")
    end)
  end

  defp check_package_req(repo, name, req, from) do
    if Registry.versions(repo, name) do
      if req != nil and Hex.Version.parse_requirement(req) == :error do
        Mix.raise "Required version #{inspect req} for package #{name} is incorrectly specified (from: #{from})"
      end
    else
      Mix.raise "No package with name #{name} (from: #{from}) in registry"
    end
  end

  defp print_success(resolved, locked) do
    locked = Enum.map(locked, &elem(&1, 0))
    resolved = Enum.into(resolved, %{}, fn {repo, name, _app, version} -> {name, {repo, version}} end)
    resolved = Map.drop(resolved, locked)

    if Map.size(resolved) != 0 do
      Hex.Shell.info "Dependency resolution completed:"
      resolved = Enum.sort(resolved)
      Enum.each(resolved, fn {name, {repo, version}} ->
        Registry.retired(repo, name, version)
        |> print_status(name, version)
      end)
    end
  end

  defp print_status(nil, name, version) do
    Hex.Shell.info IO.ANSI.format [:green, "  #{name} #{version}"]
  end

  defp print_status(retired, name, version) do
    Hex.Shell.warn "  #{name} #{version} RETIRED!"
    Hex.Shell.warn "    (#{retirement_reason(retired[:reason])}) #{retired[:message]}"
  end

  defp retirement_reason(:RETIRED_OTHER), do: "other"
  defp retirement_reason(:RETIRED_INVALID), do: "invalid"
  defp retirement_reason(:RETIRED_SECURITY), do: "security"
  defp retirement_reason(:RETIRED_DEPRECATED), do: "deprecated"
  defp retirement_reason(:RETIRED_RENAMED), do: "renamed"
  defp retirement_reason(other), do: other

  defp verify_resolved(resolved, lock) do
    Enum.each(resolved, fn {repo, name, app, version} ->
      atom_name = String.to_atom(name)

      case Hex.Utils.lock(lock[String.to_atom(app)]) do
        %{name: ^atom_name, version: ^version, repo: ^repo, checksum: checksum, deps: deps} ->
          verify_checksum(repo, name, version, checksum)
          verify_deps(repo, name, version, deps)
        _ ->
          :ok
      end
    end)
  end

  defp verify_checksum(repo, name, version, checksum) do
    registry_checksum = Registry.checksum(repo, name, version)
    if checksum && Base.decode16!(checksum, case: :lower) != registry_checksum do
      Mix.raise "Registry checksum mismatch against lock (#{name} #{version})"
    end
  end

  defp verify_deps(nil, _name, _version, _deps), do: :ok
  defp verify_deps(repo, name, version, deps) do
    deps =
      Enum.map(deps, fn {app, req, opts} ->
        {opts[:repo],
         opts[:hex],
         Atom.to_string(app),
         req,
         !!opts[:optional]}
      end)

    if Enum.sort(deps) != Enum.sort(Registry.deps(repo, name, version)),
      do: Mix.raise "Registry dependencies mismatch against lock (#{name} #{version})"
  end

  defp check_lock(lock) do
    repos = Hex.State.fetch!(:repos)
    Enum.each(lock, fn {_app, info} ->
      case Hex.Utils.lock(info) do
        %{name: name, version: version, repo: repo} ->
          check_repo(repos, repo)
          check_dep(repo, name, version)
        nil ->
          :ok
      end
    end)
  end

  defp check_dep(repo, name, version) do
    if versions = Registry.versions(repo, name) do
      unless version in versions do
        Mix.raise "Unknown package version #{name} #{version} in lockfile"
      end
    else
      Mix.raise "Unknown package #{name} in lockfile"
    end
  end

  defp check_repo(repos, repo) do
    unless Map.has_key?(repos, repo) do
      Mix.raise "Unknown repo #{repo}, add it with mix hex.repo"
    end
  end

  defp with_children(apps, lock) do
    [apps, do_with_children(apps, lock)]
    |> List.flatten
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

    unlock =
      for({app, _} <- old_lock,
          not Map.has_key?(lock, app),
        into: unlock_deps(deps, old_lock),
        do: Atom.to_string(app))
      |> Enum.uniq
      |> with_children(old_lock)
      |> Enum.uniq

    Hex.Mix.from_lock(old_lock)
    |> Enum.reject(fn {_repo, _name, app, _version} -> app in unlock end)
  end

  defp unlock_deps(deps, old_lock) do
    Enum.filter(deps, fn
      %Mix.Dep{scm: Hex.SCM, app: app, requirement: req, opts: opts} ->
        name = opts[:hex]
        case Hex.Utils.lock(old_lock[app]) do
          %{name: ^name, version: version, repo: repo} ->
            (req && !Hex.Version.match?(version, req)) ||
              (repo != opts[:repo])
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
