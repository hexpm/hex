# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.PackageLister do
  @moduledoc false

  alias Hex.Solver.{Constraint, Incompatibility, PackageLister, PackageRange, Term}
  alias Hex.Solver.Constraints.{Range, Version}

  defstruct registry: nil,
            root_dependencies: [],
            locked: [],
            overrides: [],
            already_prefetched: MapSet.new([{nil, "$lock"}]),
            already_returned: %{}

  # Prefer packages with few remaining versions so that if there is conflict
  # later it will be forced quickly
  def pick_package(lister, package_ranges) do
    {package_range, versions} =
      package_ranges
      |> Enum.sort_by(& &1.name)
      |> Enum.map(fn package_range ->
        case versions(lister, package_range.repo, package_range.name) do
          {:ok, versions} ->
            allowed = Enum.filter(versions, &Constraint.allows?(package_range.constraint, &1))
            {package_range, allowed}

          :error ->
            throw({__MODULE__, :minimal_versions, package_range})
        end
      end)
      |> Enum.min_by(fn {_package_range, versions} -> length(versions) end)

    {:ok, package_range, List.first(Enum.sort(versions, &Version.prioritize/2))}
  catch
    :throw, {__MODULE__, :minimal_versions, package_range} ->
      {:error, package_range}
  end

  def dependencies_as_incompatibilities(%PackageLister{} = lister, package_repo, package, version) do
    {:ok, versions} = versions(lister, package_repo, package)
    already_returned = Map.get(lister.already_returned, {package_repo, package}, %{})

    # Dependencies of package keyed on package version
    versions_dependencies =
      Enum.map(versions, fn version ->
        {:ok, dependencies} = dependencies(lister, package_repo, package, version)
        {version, dependencies}
      end)

    {_version, dependencies} = List.keyfind(versions_dependencies, version, 0)

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

    incompatibilities =
      Enum.map(dependencies, fn {dependency,
                                 %{
                                   repo: dependency_repo,
                                   constraint: constraint,
                                   optional: optional
                                 }} ->
        version_constraints =
          Enum.map(versions_dependencies, fn {version, dependencies} ->
            case Map.fetch(dependencies, dependency) do
              {:ok, %{constraint: constraint, optional: optional}} ->
                {version, {constraint, optional}}

              :error ->
                {version, nil}
            end
          end)

        # Find range of versions around the current version for which the
        # constraint is the same to create an incompatibility based on a
        # larger set of versions for the parent package.
        # This optimization let us skip many versions during conflict resolution.
        lower = lower_bound(Enum.reverse(version_constraints), version, {constraint, optional})
        upper = upper_bound(version_constraints, version, {constraint, optional})

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

        Incompatibility.new([package_term, dependency_term], :dependency)
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
        already_returned: Map.put(lister.already_returned, package, already_returned)
    }

    {lister, incompatibilities}
  end

  defp lower_bound(versions_dependencies, version, constraint) do
    [{version, _} | versions_dependencies] = skip_to_version(versions_dependencies, version)

    skip_to_last_constraint(versions_dependencies, constraint, version)
  end

  defp upper_bound(versions_dependencies, version, constraint) do
    versions_dependencies = skip_to_version(versions_dependencies, version)
    skip_to_after_constraint(versions_dependencies, constraint)
  end

  defp skip_to_version([{version, _constraint} | _] = versions_dependencies, version) do
    versions_dependencies
  end

  defp skip_to_version([_ | versions_dependencies], version) do
    skip_to_version(versions_dependencies, version)
  end

  defp skip_to_last_constraint(
         [{version, constraint} | versions_dependencies],
         {_constraint, _optional} = constraint,
         _last
       ) do
    skip_to_last_constraint(versions_dependencies, constraint, version)
  end

  defp skip_to_last_constraint([], _constraint, _last) do
    nil
  end

  defp skip_to_last_constraint(_versions_dependencies, _constraint, last) do
    last
  end

  defp skip_to_after_constraint(
         [{_, constraint}, {version, constraint} | versions_dependencies],
         {_constraint, _optional} = constraint
       ) do
    skip_to_after_constraint([{version, constraint} | versions_dependencies], constraint)
  end

  defp skip_to_after_constraint([_, {version, _} | _versions_dependencies], _) do
    version
  end

  defp skip_to_after_constraint(_, _constraint) do
    nil
  end

  @version_1 Elixir.Version.parse!("1.0.0")

  defp versions(_lister, _repo, "$root"), do: {:ok, [@version_1]}
  defp versions(_lister, _repo, "$lock"), do: {:ok, [@version_1]}

  defp versions(%PackageLister{} = lister, repo, package) do
    root_dependency = find_root_dependency(lister, repo, package)

    if root_dependency do
      {:ok, [@version_1]}
    else
      lister.registry.versions(repo, package)
    end
  end

  defp dependencies(%PackageLister{} = lister, _repo, "$root", @version_1) do
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

  defp dependencies(%PackageLister{locked: locked}, _repo, "$lock", @version_1) do
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

  defp dependencies(%PackageLister{} = lister, repo, package, version) do
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

  defp find_root_dependency(%PackageLister{root_dependencies: dependencies}, repo, package) do
    Enum.find(dependencies, &(&1.repo == repo and &1.name == package and &1.dependencies != []))
  end
end
