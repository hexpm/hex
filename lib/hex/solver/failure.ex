# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Failure do
  @moduledoc false

  alias Hex.Solver.Incompatibility

  def write(root, opts \\ []) do
    derivations = count_derivations(%{}, root)
    state = state(root, derivations, opts)

    state =
      case root.cause do
        {:conflict, _, _} ->
          visit(state, root)

        _ ->
          write(state, root, false, [
            "Because ",
            Incompatibility.to_string(root, ansi: state.ansi),
            " version solving failed."
          ])
      end

    numbers =
      state.lines
      |> Enum.map(fn {number, _message} -> number end)
      |> Enum.filter(&is_integer/1)

    indent? = numbers != []
    indent = if indent?, do: Enum.max_by(numbers, &byte_size(Integer.to_string(&1))) + 3

    Enum.map_join(Enum.reverse(state.lines), "\n", fn {number, message} ->
      if number do
        "(#{number}) #{message}"
      else
        "#{message && indent && String.duplicate(" ", indent)}#{message}"
      end
    end)
  end

  defp count_derivations(derivations, incompatibility) do
    case Map.fetch(derivations, incompatibility) do
      {:ok, value} ->
        Map.put(derivations, incompatibility, value + 1)

      :error ->
        derivations =
          case incompatibility.cause do
            {:conflict, conflict, other} ->
              derivations
              |> count_derivations(conflict)
              |> count_derivations(other)

            _ ->
              derivations
          end

        Map.update(derivations, incompatibility, 1, &(&1 + 1))
    end
  end

  defp visit(state, incompatibility, conclusion? \\ false) do
    numbered? = conclusion? or Map.fetch!(state.derivations, incompatibility) > 1
    conjunction = if conclusion? or incompatibility == state.root, do: "So,", else: "And"
    incompatibility_string = Incompatibility.to_string(incompatibility, ansi: state.ansi)
    {:conflict, conflict, other} = incompatibility.cause

    case {conflict.cause, other.cause} do
      {{:conflict, _, _}, {:conflict, _, _}} ->
        conflict_line = state.line_numbers[conflict]
        other_line = state.line_numbers[other]
        single_line_conflict? = single_line?(conflict.cause)
        single_line_other? = single_line?(other.cause)

        cond do
          conflict_line && other_line ->
            write(state, incompatibility, numbered?, [
              "Because ",
              Incompatibility.to_string_and(conflict, other,
                left_line: conflict_line,
                right_line: other_line,
                ansi: state.ansi
              ),
              ", ",
              incompatibility_string
            ])

          conflict_line || other_line ->
            {with_line, without_line, line} =
              if conflict_line do
                {conflict, other, conflict_line}
              else
                {other, conflict, other_line}
              end

            state
            |> visit(without_line)
            |> write(incompatibility, numbered?, [
              conjunction,
              " because ",
              Incompatibility.to_string(with_line, ansi: state.ansi),
              " (#{line}), ",
              incompatibility_string
            ])

          single_line_conflict? or single_line_other? ->
            first = if single_line_other?, do: conflict, else: other
            second = if single_line_other?, do: other, else: conflict

            state
            |> visit(first)
            |> visit(second)
            |> write(incompatibility, numbered?, ["This, ", incompatibility_string])

          true ->
            state
            |> visit(conflict, _conclusion? = true)
            |> add_line()
            |> visit(other)
            |> write(incompatibility, numbered?, fn state ->
              [
                conjunction,
                " because ",
                Incompatibility.to_string(conflict, ansi: state.ansi),
                maybe_line(state.line_numbers[conflict]),
                ", ",
                incompatibility_string,
                "."
              ]
            end)
        end

      {left, right} when elem(left, 0) == :conflict when elem(right, 0) == :conflict ->
        derived = if match?({:conflict, _, _}, conflict.cause), do: conflict, else: other
        ext = if match?({:conflict, _, _}, conflict.cause), do: other, else: conflict

        cond do
          derived_line = state.line_numbers[derived] ->
            write(state, incompatibility, numbered?, [
              "Because ",
              Incompatibility.to_string_and(ext, derived,
                right_line: derived_line,
                ansi: state.ansi
              ),
              ", ",
              incompatibility_string
            ])

          collapsible?(state, derived) ->
            {:conflict, derived_conflict, derived_other} = derived.cause

            collapsed_derived =
              if match?({:conflict, _, _}, derived_conflict.cause),
                do: derived_conflict,
                else: derived_other

            collapsed_ext =
              if match?({:conflict, _, _}, derived_conflict.cause),
                do: derived_other,
                else: derived_conflict

            state
            |> visit(collapsed_derived)
            |> write(incompatibility, numbered?, [
              conjunction,
              " because ",
              Incompatibility.to_string_and(collapsed_ext, ext, ansi: state.ansi),
              ", ",
              incompatibility_string,
              "."
            ])

          true ->
            state
            |> visit(derived)
            |> write(incompatibility, numbered?, [
              conjunction,
              " because ",
              Incompatibility.to_string(ext, ansi: state.ansi),
              ", ",
              incompatibility_string,
              "."
            ])
        end

      _ ->
        write(state, incompatibility, numbered?, [
          "Because ",
          Incompatibility.to_string_and(conflict, other, ansi: state.ansi),
          ", ",
          incompatibility_string,
          "."
        ])
    end
  end

  defp write(state, incompatibility, numbered?, message) do
    message = maybe_fun(state, message)

    if numbered? do
      number = map_size(state.line_numbers) + 1

      %{
        state
        | lines: [{number, message} | state.lines],
          line_numbers: Map.put(state.line_numbers, incompatibility, number)
      }
    else
      %{state | lines: [{nil, message} | state.lines]}
    end
  end

  defp add_line(state, message \\ nil) do
    %{state | lines: [{nil, message} | state.lines]}
  end

  defp maybe_fun(state, fun) when is_function(fun, 1), do: fun.(state)
  defp maybe_fun(_state, other), do: other

  defp maybe_line(nil), do: ""
  defp maybe_line(line), do: " (#{line})"

  defp collapsible?(
         state,
         %Incompatibility{cause: {:conflict, conflict, other}} = incompatibility
       ) do
    complex = if match?({:conflict, _, _}, conflict), do: conflict, else: other

    cond do
      state.derivations[incompatibility] > 1 -> false
      match?({:conflict, _, _}, conflict) and match?({:conflict, _, _}, other) -> false
      not match?({:conflict, _, _}, conflict) and not match?({:conflict, _, _}, other) -> false
      true -> not Map.has_key?(state.line_numbers, complex)
    end
  end

  defp single_line?({
         :conflict,
         %Incompatibility{cause: {:conflict, _, _}},
         %Incompatibility{cause: {:conflict, _, _}}
       }),
       do: true

  defp single_line?({:conflict, %Incompatibility{}, %Incompatibility{}}), do: false

  defp state(root, derivations, opts) do
    %{
      root: root,
      lines: [],
      line_numbers: %{},
      derivations: derivations,
      ansi: Keyword.get(opts, :ansi, false)
    }
  end
end
