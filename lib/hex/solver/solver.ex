# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Solver do
  @moduledoc false

  alias Hex.Solver.{
    Incompatibility,
    PackageLister,
    PackageRange,
    PartialSolution,
    Term
  }

  alias Hex.Solver.Constraints.Util

  require Logger

  def run(registry, dependencies, locked, overrides) do
    solve("$root", new_state(registry, dependencies, locked, overrides))
  end

  defp solve(next, state) do
    case unit_propagation([next], state) do
      {:ok, state} ->
        case choose_package_version(state) do
          :done -> {:ok, state.solution.decisions}
          {:choice, package, state} -> solve(package, state)
        end

      {:error, incompatibility, _state} ->
        {:error, incompatibility}
    end
  end

  defp unit_propagation([], state) do
    {:ok, state}
  end

  defp unit_propagation([package | changed], state) do
    incompatibilities = Map.fetch!(state.incompatibilities, package)

    result =
      Enum.reduce_while(incompatibilities, {changed, state}, fn
        incompatibility, {changed, state} ->
          case propagate_incompatibility(incompatibility.terms, nil, incompatibility, state) do
            {:ok, result, state} -> {:cont, {changed ++ [result], state}}
            {:error, :none} -> {:cont, {changed, state}}
            {:error, :conflict} -> unit_propagation_conflict(incompatibility, state)
          end
      end)

    case result do
      {:error, incompatibility, state} -> {:error, incompatibility, state}
      {changed, state} -> unit_propagation(changed, state)
    end
  end

  defp unit_propagation_conflict(incompatibility, state) do
    case conflict_resolution(state, incompatibility) do
      {:ok, root_cause, state} ->
        {:ok, result, state} =
          propagate_incompatibility(root_cause.terms, nil, root_cause, state, optionals: false)

        {:halt, {[result], state}}

      {:error, incompatibility} ->
        {:halt, {:error, incompatibility, state}}
    end
  end

  defp propagate_incompatibility(terms, unsatisified, incompatibility, state, opts \\ [])

  defp propagate_incompatibility([term | terms], unsatisified, incompatibility, state, opts) do
    case PartialSolution.relation(state.solution, term, opts) do
      :disjoint ->
        # If the term is contradicted by the partial solution then the
        # incompatibility is also contradicted so we can deduce nothing
        {:error, :none}

      :overlapping when unsatisified != nil ->
        # If more than one term is inconclusive we can deduce nothing
        {:error, :none}

      :overlapping ->
        propagate_incompatibility(terms, term, incompatibility, state, opts)

      :subset ->
        propagate_incompatibility(terms, unsatisified, incompatibility, state, opts)
    end
  end

  defp propagate_incompatibility([], nil, _incompatibility, _state, _opts) do
    # All terms in the incompatibility are satisified by the partial solution
    # so we have a conflict
    {:error, :conflict}
  end

  defp propagate_incompatibility([], unsatisfied, incompatibility, state, _opts) do
    # Only one term in the incompatibility was unsatisfied
    unsatisfied = %{unsatisfied | positive: not unsatisfied.positive}
    Logger.debug("RESOLVER: derived #{unsatisfied}")

    solution = PartialSolution.derive(state.solution, unsatisfied, incompatibility)
    {:ok, unsatisfied.package_range.name, %{state | solution: solution}}
  end

  defp choose_package_version(state) do
    unsatisfied = PartialSolution.unsatisfied(state.solution)

    if unsatisfied == [] do
      :done
    else
      case PackageLister.pick_package(state.lister, unsatisfied) do
        {:ok, package_range, nil} ->
          # If no version satisfies the constraint then add an incompatibility that indicates that
          term = %Term{positive: true, package_range: package_range}
          incompatibility = Incompatibility.new([term], :no_versions)
          state = add_incompatibility(state, incompatibility)
          {:choice, package_range.name, state}

        {:ok, package_range, version} ->
          {lister, incompatibilities} =
            PackageLister.dependencies_as_incompatibilities(
              state.lister,
              package_range.repo,
              package_range.name,
              version
            )

          state = %{state | lister: lister}

          {state, conflict} =
            Enum.reduce(incompatibilities, {state, false}, fn incompatibility,
                                                              {state, conflict} ->
              # If an incompatibility is already satisfied then selecting this version would cause
              # a conflict. We'll continue adding its dependencies then go back to unit propagation
              # that will eventually choose a better version.
              conflict =
                conflict or
                  incompatibility_conflict?(state, incompatibility, package_range.name)

              state = add_incompatibility(state, incompatibility)
              {state, conflict}
            end)

          solution =
            if conflict do
              state.solution
            else
              package_range = %PackageRange{package_range | constraint: version}
              Logger.debug("RESOLVER: selecting #{package_range}")
              PartialSolution.decide(state.solution, package_range)
            end

          state = %{state | solution: solution}
          {:choice, package_range.name, state}

        {:error, package_range} ->
          package_range = %PackageRange{package_range | constraint: Util.any()}
          term = %Term{positive: true, package_range: package_range}
          incompatibility = Incompatibility.new([term], :package_not_found)
          state = add_incompatibility(state, incompatibility)
          {:choice, package_range.name, state}
      end
    end
  end

  defp add_incompatibility(state, incompatibility) do
    Logger.debug("RESOLVER: fact #{incompatibility}")

    incompatibilities =
      Enum.reduce(incompatibility.terms, state.incompatibilities, fn term, incompatibilities ->
        Map.update(
          incompatibilities,
          term.package_range.name,
          [incompatibility],
          &[incompatibility | &1]
        )
      end)

    %{state | incompatibilities: incompatibilities}
  end

  defp incompatibility_conflict?(state, incompatibility, name) do
    Enum.all?(incompatibility.terms, fn term ->
      term.package_range.name == name or PartialSolution.satisfies?(state.solution, term)
    end)
  end

  # Given an incompatibility that's satisified by the solution, construct a new
  # incompatibility that encapsulates the root cause of the conflict and backtracks
  # until the new incompatibility will allow propagation to deduce new assignments.
  defp conflict_resolution(state, incompatibility) do
    Logger.debug("RESOLVER: conflict #{incompatibility}")
    do_conflict_resolution(state, incompatibility, false)
  catch
    :throw, {__MODULE__, :conflict_resolution, incompatibility, state} ->
      {:ok, incompatibility, state}
  end

  defp do_conflict_resolution(state, incompatibility, new_incompatibility?) do
    if Incompatibility.failure?(incompatibility) do
      {:error, incompatibility}
    else
      resolution =
        Enum.reduce(incompatibility.terms, new_resolution_state(), fn term, resolution ->
          satisfier = PartialSolution.satisfier(state.solution, term, optionals: false)

          resolution =
            cond do
              resolution.most_recent_satisfier == nil ->
                %{resolution | most_recent_term: term, most_recent_satisfier: satisfier}

              resolution.most_recent_satisfier.index < satisfier.index ->
                %{
                  resolution
                  | most_recent_term: term,
                    most_recent_satisfier: satisfier,
                    difference: nil,
                    previous_satisfier_level:
                      max(
                        resolution.previous_satisfier_level,
                        resolution.most_recent_satisfier.decision_level
                      )
                }

              true ->
                %{
                  resolution
                  | previous_satisfier_level:
                      max(resolution.previous_satisfier_level, satisfier.decision_level)
                }
            end

          if resolution.most_recent_term == term do
            # If most_recent_satisfier doesn't satisfy most_recent_term on its own,
            # then the next most recent satisfier may not be the one that satisfies
            # remainder

            difference =
              Term.difference(resolution.most_recent_satisfier.term, resolution.most_recent_term)

            if difference do
              satisfier =
                PartialSolution.satisfier(
                  state.solution,
                  Term.inverse(difference),
                  optionals: false
                )

              previous_satisfier_level =
                max(resolution.previous_satisfier_level, satisfier.decision_level)

              %{
                resolution
                | difference: difference,
                  previous_satisfier_level: previous_satisfier_level
              }
            else
              %{resolution | difference: difference}
            end
          else
            resolution
          end
        end)

      # If most_recent_satisfier is the only satisfier left at its decision level,
      # or if it has no cause (indicating that it's a decision rather than a
      # derivation), then the incompatibility is the root cause. We then backjump
      # to previous_satisfier_level, where the incompatibility is guaranteed to
      # allow propagation to produce more assignments

      if resolution.previous_satisfier_level < resolution.most_recent_satisfier.decision_level or
           resolution.most_recent_satisfier.cause == nil do
        solution = PartialSolution.backtrack(state.solution, resolution.previous_satisfier_level)
        state = %{state | solution: solution}

        state =
          if new_incompatibility?,
            do: add_incompatibility(state, incompatibility),
            else: state

        throw({__MODULE__, :conflict_resolution, incompatibility, state})
      end

      # Create a new incompatibility by combining the given incompatibility with
      # the incompatibility that caused most_recent_satisfier to be assigned.
      # Doing this iteratively constructs a new new incompatibility that's guaranteed
      # to be true (we know for sure no solution will satisfy the incompatibility)
      # while also approximating the intuitive notion of the "root cause" of the conflict.

      new_terms =
        Enum.filter(incompatibility.terms, &(&1 != resolution.most_recent_term)) ++
          Enum.filter(
            resolution.most_recent_satisfier.cause.terms,
            &(&1.package_range != resolution.most_recent_satisfier.term.package_range)
          )

      # The most_recent_satisfier may not satisfy most_recent_term on its own if
      # there are a collection of constraints on most_recent_term that only satisfy
      # it together. For example, if most_recent_term is `foo ~> 1.0` and solution
      # contains `[foo >= 1.0.0, foo < 2.0.0]`, the most_recent_satisfier will be
      # `foo < 2.0.0` even though it doesn't totally satisfy `foo ~> 1.0`.

      # In this case we add `most_recent_satisfier \ most_recent_term` to the
      # incompatibility as well. See https://github.com/dart-lang/pub/tree/master/doc/solver.md#conflict-resolution
      # for more details.

      new_terms =
        if resolution.difference,
          do: new_terms ++ [Term.inverse(resolution.difference)],
          else: new_terms

      incompatibility =
        Incompatibility.new(
          new_terms,
          {:conflict, incompatibility, resolution.most_recent_satisfier.cause}
        )

      partially = if resolution.difference, do: " partially"

      Logger.debug("""
      RESOLVER: conflict resolution
        #{resolution.most_recent_term} is#{partially} satisfied by #{resolution.most_recent_satisfier}
        which is caused by #{resolution.most_recent_satisfier.cause}
        thus #{incompatibility}\
      """)

      do_conflict_resolution(state, incompatibility, true)
    end
  end

  defp new_resolution_state() do
    %{
      # The term in incompatibility.terms that was most recently satisfied by the solution.
      most_recent_term: nil,
      # The earliest assignment in the solution such that incompatibility is satisfied
      # by the solution up to and including this assignment.
      most_recent_satisfier: nil,
      # The difference between most_recent_satisfier and most_recent_term, that is,
      # the versions that are allowed by most_recent_satisfier but not by most_recent_term.
      # nil if most_recent_satisfier totally satisfies most_recent_term.
      difference: nil,
      # The decision level of the earliest assignment before most_recent_satisfier
      # such that incompatibility is satisfied by the solution up to and including
      # this assignment and most_recent_satisfier.
      # Decision level 1 is the level where the root package was selected. We can
      # go back to level 0 but level 1 tends to give better error messages, because
      # references to the root package end up closer to the final conclusion that
      # no solution exists.
      previous_satisfier_level: 1
    }
  end

  defp new_state(registry, root_dependencies, locked, overrides) do
    version = Version.parse!("1.0.0")
    package_range = %PackageRange{name: "$root", constraint: version}
    root = Incompatibility.new([%Term{positive: false, package_range: package_range}], :root)

    locked =
      Enum.map(locked, fn %{repo: repo, name: package, version: version, label: label} ->
        %{repo: repo, name: package, constraint: version, label: label}
      end)

    lister = %PackageLister{
      registry: registry,
      root_dependencies: root_dependencies,
      locked: locked,
      overrides: overrides
    }

    %{
      solution: %PartialSolution{},
      incompatibilities: %{},
      lister: lister
    }
    |> add_incompatibility(root)
  end
end
