# Vendored from hex_solver v0.3.1 (2a24512), do not edit manually

defmodule Hex.Solver.PackageLister do
  @moduledoc false

  alias Hex.Solver.{Constraint, Incompatibility, PackageLister, PackageRange, Term}
  alias Hex.Solver.Constraints.{Range, Version}

  defstruct registry: nil,
            root_dependencies: [],
            root_parents: %{},
            locked: [],
            overrides: [],
            already_prefetched: MapSet.new([{nil, "$lock"}]),
            already_returned: %{},
            versions: %{},
            candidates: %{},
            dependencies: %{}

  def new(registry, root_dependencies, locked, overrides) do
    %PackageLister{
      registry: registry,
      root_dependencies: root_dependencies,
      root_parents: root_parents(root_dependencies),
      locked: locked,
      overrides: overrides
    }
  end

  # Prefer packages with few remaining versions so that if there is conflict
  # later it will be forced quickly
  def pick_package(lister, package_ranges) do
    pick_package(lister, Enum.sort_by(package_ranges, & &1.name), nil)
  end

  defp pick_package(lister, [package_range | package_ranges], best) do
    case candidates(lister, package_range) do
      {lister, {:ok, count, version}} ->
        best =
          if best == nil or count < elem(best, 1),
            do: {package_range, count, version},
            else: best

        pick_package(lister, package_ranges, best)

      {lister, :error} ->
        {:error, lister, package_range}
    end
  end

  defp pick_package(lister, [], {package_range, _count, version}) do
    {:ok, lister, package_range, version}
  end

  # The number of versions a package range allows and the preferred one. The
  # same package ranges stay unsatisfied over many decisions, so cache them.
  defp candidates(lister, %PackageRange{repo: repo, name: name, constraint: constraint}) do
    key = {repo, name, constraint}

    case lister.candidates do
      %{^key => candidates} ->
        {lister, candidates}

      %{} ->
        {lister, versions} = versions(lister, repo, name)

        candidates =
          case versions do
            {:ok, versions} ->
              allowed = Enum.filter(versions, &Constraint.allows?(constraint, &1))
              {:ok, length(allowed), List.first(Enum.sort(allowed, &Version.prioritize/2))}

            :error ->
              :error
          end

        {%{lister | candidates: Map.put(lister.candidates, key, candidates)}, candidates}
    end
  end

  def dependencies_as_incompatibilities(%PackageLister{} = lister, package_repo, package, version) do
    {lister, {:ok, versions}} = versions(lister, package_repo, package)
    {lister, {:ok, dependencies}} = dependencies(lister, package_repo, package, version)
    already_returned = Map.get(lister.already_returned, {package_repo, package}, %{})

    # Dependencies of package version filtered of overrides and already returned dependencies
    dependencies =
      dependencies
      |> Enum.sort()
      |> Enum.reject(fn {_dependency, %{label: label}} ->
        package not in ["$root", "$lock"] and label in lister.overrides
      end)
      |> Enum.reject(fn {dependency, _} ->
        case Map.fetch(already_returned, dependency) do
          {:ok, returned_constraint} -> Constraint.allows?(returned_constraint, version)
          :error -> false
        end
      end)

    index = Enum.find_index(versions, &(&1 == version))
    versions = List.to_tuple(versions)

    {incompatibilities, lister} =
      Enum.map_reduce(dependencies, lister, fn {dependency,
                                                %{
                                                  repo: dependency_repo,
                                                  constraint: constraint,
                                                  optional: optional
                                                }},
                                               lister ->
        # Find range of versions around the current version for which the
        # constraint is the same to create an incompatibility based on a
        # larger set of versions for the parent package.
        # This optimization let us skip many versions during conflict resolution.
        run = %{
          repo: package_repo,
          package: package,
          versions: versions,
          dependency: dependency,
          constraint: {constraint, optional}
        }

        {lister, lower} = lower_bound(lister, run, index - 1, version)
        {lister, upper} = upper_bound(lister, run, index + 1)

        range = %Range{min: lower, max: upper, include_min: !!lower}
        package_range = %PackageRange{repo: package_repo, name: package, constraint: range}
        package_term = %Term{positive: true, package_range: package_range}

        dependency_range = %PackageRange{
          repo: dependency_repo,
          name: dependency,
          constraint: constraint
        }

        dependency_term = %Term{
          positive: false,
          package_range: dependency_range,
          optional: optional
        }

        {Incompatibility.new([package_term, dependency_term], :dependency), lister}
      end)

    prefetch =
      dependencies
      |> Enum.filter(fn {_dependency, %{prefetch: prefetch}} -> prefetch end)
      |> MapSet.new(fn {dependency, %{repo: repo}} -> {repo, dependency} end)
      |> MapSet.difference(lister.already_prefetched)

    if MapSet.size(prefetch) > 0 do
      lister.registry.prefetch(Enum.to_list(prefetch))
    end

    # Dependencies already returned
    already_returned =
      Enum.reduce(incompatibilities, already_returned, fn incompatibility, already_returned ->
        [package_term, dependency_term] =
          case incompatibility do
            %Incompatibility{terms: [single]} -> [single, single]
            %Incompatibility{terms: [package, dependency]} -> [package, dependency]
          end

        name = dependency_term.package_range.name
        constraint = package_term.package_range.constraint
        Map.update(already_returned, name, constraint, &Constraint.union(&1, constraint))
      end)

    lister = %{
      lister
      | already_prefetched: MapSet.union(lister.already_prefetched, prefetch),
        already_returned:
          Map.put(lister.already_returned, {package_repo, package}, already_returned)
    }

    {lister, incompatibilities}
  end

  # The lowest version in the run of versions below the index that depend on
  # the dependency with the same constraint, or nil if the run reaches the
  # first version
  defp lower_bound(lister, _run, -1, _last) do
    {lister, nil}
  end

  defp lower_bound(lister, run, index, last) do
    case same_constraint?(lister, run, index) do
      {lister, true} -> lower_bound(lister, run, index - 1, elem(run.versions, index))
      {lister, false} -> {lister, last}
    end
  end

  # The first version after the run of versions from the index that depend on
  # the dependency with the same constraint, or nil if the run reaches the last
  # version
  defp upper_bound(lister, run, index) when index == tuple_size(run.versions) do
    {lister, nil}
  end

  defp upper_bound(lister, run, index) do
    case same_constraint?(lister, run, index) do
      {lister, true} -> upper_bound(lister, run, index + 1)
      {lister, false} -> {lister, elem(run.versions, index)}
    end
  end

  # Loads the dependencies of the version, so only the versions in a run and
  # the one that ends it are loaded
  defp same_constraint?(lister, run, index) do
    version = elem(run.versions, index)
    {lister, {:ok, dependencies}} = dependencies(lister, run.repo, run.package, version)
    dependency = run.dependency

    case dependencies do
      %{^dependency => %{constraint: constraint, optional: optional}} ->
        {lister, {constraint, optional} == run.constraint}

      %{} ->
        {lister, false}
    end
  end

  @version_1 Elixir.Version.parse!("1.0.0")

  # Fetch each package's versions once per solve, so the solver works with the
  # same versions throughout even if the registry filters them by time
  defp versions(%PackageLister{} = lister, repo, package) do
    key = {repo, package}

    case lister.versions do
      %{^key => result} ->
        {lister, result}

      %{} ->
        result = uncached_versions(lister, repo, package)
        {%{lister | versions: Map.put(lister.versions, key, result)}, result}
    end
  end

  defp uncached_versions(_lister, _repo, "$root"), do: {:ok, [@version_1]}
  defp uncached_versions(_lister, _repo, "$lock"), do: {:ok, [@version_1]}

  defp uncached_versions(%PackageLister{} = lister, repo, package) do
    root_dependency = find_root_dependency(lister, repo, package)

    if root_dependency do
      {:ok, [@version_1]}
    else
      lister.registry.versions(repo, package)
    end
  end

  # Dependencies are loaded as the solver needs them and kept for the solve
  defp dependencies(%PackageLister{} = lister, repo, package, version) do
    key = {repo, package, version}

    case lister.dependencies do
      %{^key => result} ->
        {lister, result}

      %{} ->
        result = uncached_dependencies(lister, repo, package, version)
        {%{lister | dependencies: Map.put(lister.dependencies, key, result)}, result}
    end
  end

  defp uncached_dependencies(%PackageLister{} = lister, _repo, "$root", @version_1) do
    lock_dependency =
      if lister.locked == [] do
        []
      else
        [%{repo: nil, name: "$lock", constraint: @version_1, optional: false, label: "$lock"}]
      end

    root_dependencies =
      Enum.map(lister.root_dependencies, fn dependency ->
        Map.put(dependency, :prefetch, dependency.dependencies == [])
      end)

    {:ok, dependency_map(lock_dependency ++ root_dependencies)}
  end

  defp uncached_dependencies(%PackageLister{locked: locked}, _repo, "$lock", @version_1) do
    {:ok,
     Map.new(locked, fn dependency ->
       {dependency.name,
        %{
          repo: dependency.repo,
          constraint: dependency.constraint,
          optional: true,
          label: dependency.label,
          prefetch: true
        }}
     end)}
  end

  defp uncached_dependencies(%PackageLister{} = lister, repo, package, version) do
    root_dependency = find_root_dependency(lister, repo, package)

    if root_dependency do
      {:ok, dependency_map(root_dependency.dependencies)}
    else
      case lister.registry.dependencies(repo, package, version) do
        {:ok, dependencies} -> {:ok, dependency_map(dependencies)}
        :error -> :error
      end
    end
  end

  defp dependency_map(dependencies) do
    Map.new(dependencies, fn dependency ->
      {dependency.name,
       %{
         repo: dependency.repo,
         constraint: dependency.constraint,
         optional: dependency.optional,
         label: dependency.label,
         prefetch: Map.get(dependency, :prefetch, true)
       }}
    end)
  end

  # Root dependencies with sub dependencies stand in for registry packages.
  # Index them by package, keeping the first declaration of each.
  defp root_parents(dependencies) do
    Enum.reduce(dependencies, %{}, fn
      %{dependencies: []}, parents -> parents
      dependency, parents -> Map.put_new(parents, {dependency.repo, dependency.name}, dependency)
    end)
  end

  defp find_root_dependency(%PackageLister{root_parents: parents}, repo, package) do
    Map.get(parents, {repo, package})
  end
end
