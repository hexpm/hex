# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.PartialSolution do
  @moduledoc false

  alias Hex.Solver.{Assignment, PackageRange, PartialSolution, Term}

  defstruct assignments: [],
            decisions: %{},
            positive: %{},
            negative: %{},
            backtracking: false,
            attempted_solutions: 1

  def relation(%PartialSolution{} = solution, %Term{} = term, opts \\ []) do
    name = term.package_range.name

    case Map.fetch(solution.positive, name) do
      {:ok, positive} ->
        Term.relation(positive.term, term, opts)

      :error ->
        case Map.fetch(solution.negative, name) do
          {:ok, negative} -> Term.relation(negative.term, term, opts)
          :error -> :overlapping
        end
    end
  end

  def satisfies?(%PartialSolution{} = solution, %Term{} = term, opts \\ []) do
    relation(solution, term, opts) == :subset
  end

  def satisfier(%PartialSolution{} = solution, term, opts \\ []) do
    {:satisfier, assignment} =
      Enum.reduce_while(Enum.reverse(solution.assignments), nil, fn assignment, assigned_term ->
        if assignment.term.package_range.name == term.package_range.name do
          if Term.compatible_package?(assignment.term, term) do
            assigned_term =
              if assigned_term do
                Assignment.intersect(assigned_term, assignment)
              else
                assignment
              end

            if Term.satisfies?(assigned_term.term, term, opts) do
              {:halt, {:satisfier, assignment}}
            else
              {:cont, assigned_term}
            end
          else
            if assignment.term.positive do
              false = term.positive
              {:halt, {:satisfier, assignment}}
            else
              {:cont, assigned_term}
            end
          end
        else
          {:cont, assigned_term}
        end
      end)

    assignment
  end

  def backtrack(%PartialSolution{} = solution, decision_level) do
    {removed, assignments} =
      Enum.split_while(solution.assignments, &(&1.decision_level > decision_level))

    removed_names = Enum.map(removed, & &1.term.package_range.name)
    removed_decisions = Enum.filter(removed, &Assignment.decision?/1)

    decisions =
      Map.drop(solution.decisions, Enum.map(removed_decisions, & &1.term.package_range.name))

    positive = Map.drop(solution.positive, removed_names)
    negative = Map.drop(solution.negative, removed_names)

    solution = %{
      solution
      | assignments: assignments,
        decisions: decisions,
        positive: positive,
        negative: negative,
        backtracking: true
    }

    Enum.reduce(assignments, solution, fn assignment, solution ->
      if assignment.term.package_range.name in removed_names do
        register(solution, assignment)
      else
        solution
      end
    end)
  end

  def derive(%PartialSolution{} = solution, term, incompatibility) do
    {assignment, solution} = assign(solution, term, incompatibility)
    register(solution, assignment)
  end

  def decide(
        %PartialSolution{} = solution,
        %PackageRange{repo: repo, name: package, constraint: version} = package_range
      ) do
    attempted_solutions = solution.attempted_solutions + if solution.backtracking, do: 1, else: 0
    decisions = Map.put(solution.decisions, package, {version, repo})
    term = %Term{package_range: package_range, positive: true}

    solution = %{
      solution
      | attempted_solutions: attempted_solutions,
        backtracking: false,
        decisions: decisions
    }

    {assignment, solution} = assign(solution, term, nil)
    register(solution, assignment)
  end

  defp assign(solution, term, incompatibility) do
    assignment = %Assignment{
      term: term,
      decision_level: map_size(solution.decisions),
      index: length(solution.assignments),
      cause: incompatibility
    }

    solution = %{solution | assignments: [assignment | solution.assignments]}
    {assignment, solution}
  end

  defp register(solution, assignment) do
    name = assignment.term.package_range.name

    case Map.fetch(solution.positive, name) do
      {:ok, old_assignment} ->
        assignment = Assignment.intersect(old_assignment, assignment)
        positive = Map.put(solution.positive, name, assignment)
        %{solution | positive: positive}

      :error ->
        assignment =
          if old_assignment = Map.get(solution.negative, name) do
            Assignment.intersect(old_assignment, assignment)
          else
            assignment
          end

        if assignment.term.positive do
          negative = Map.delete(solution.negative, name)
          positive = Map.put(solution.positive, name, assignment)
          %{solution | negative: negative, positive: positive}
        else
          negative = Map.put(solution.negative, name, assignment)
          %{solution | negative: negative}
        end
    end
  end

  def unsatisfied(%PartialSolution{} = solution) do
    solution.positive
    |> Map.values()
    |> Enum.reject(&Map.has_key?(solution.decisions, &1.term.package_range.name))
    |> Enum.reject(& &1.term.optional)
    |> Enum.map(& &1.term.package_range)
  end
end
