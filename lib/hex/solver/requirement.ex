# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Requirement do
  @moduledoc false

  alias Hex.Solver.Constraints.{Range, Util}
  alias Hex.Solver.Requirement.Parser

  @allowed_range_ops [:>, :>=, :<, :<=, :~>]

  def to_constraint(string) when is_binary(string) do
    case Parser.parse(string) do
      {:ok, lexed} -> {:ok, delex(lexed, [])}
      :error -> :error
    end
  catch
    {__MODULE__, :invalid_constraint} ->
      :error
  end

  def to_constraint(%Elixir.Version{} = version) do
    {:ok, version}
  end

  def to_constraint(%Elixir.Version.Requirement{} = requirement) do
    to_constraint(to_string(requirement))
  end

  def to_constraint!(string) when is_binary(string) do
    case Parser.parse(string) do
      {:ok, lexed} -> delex(lexed, [])
      :error -> raise Elixir.Version.InvalidRequirementError, string
    end
  catch
    {__MODULE__, :invalid_constraint} ->
      raise Elixir.Version.InvalidRequirementError, string
  end

  def to_constraint!(%Elixir.Version{} = version) do
    %{version | build: nil}
  end

  def to_constraint!(%Elixir.Version.Requirement{} = requirement) do
    to_constraint!(to_string(requirement))
  end

  defp delex([], acc) do
    Util.union(acc)
  end

  defp delex([op | rest], acc) when op in [:||, :or] do
    delex(rest, acc)
  end

  defp delex([op1, version1, op, op2, version2 | rest], acc) when op in [:&&, :and] do
    range = to_range(op1, version1, op2, version2)
    delex(rest, [range | acc])
  end

  defp delex([op, version | rest], acc) do
    range = to_range(op, version)
    delex(rest, [range | acc])
  end

  defp to_range(:==, version) do
    to_version(version)
  end

  defp to_range(:~>, {major, minor, nil, pre, _build}) do
    %Range{
      min: to_version({major, minor, 0, pre, nil}),
      max: to_version({major + 1, 0, 0, [0], nil}),
      include_min: true
    }
  end

  defp to_range(:~>, {major, minor, patch, pre, _build}) do
    %Range{
      min: to_version({major, minor, patch, pre, nil}),
      max: to_version({major, minor + 1, 0, [0], nil}),
      include_min: true
    }
  end

  defp to_range(:>, version) do
    %Range{min: to_version(version)}
  end

  defp to_range(:>=, version) do
    %Range{min: to_version(version), include_min: true}
  end

  defp to_range(:<, version) do
    %Range{max: to_version(version)}
  end

  defp to_range(:<=, version) do
    %Range{max: to_version(version), include_max: true}
  end

  defp to_range(:~>, _version1, :~>, _version2) do
    throw({__MODULE__, :invalid_constraint})
  end

  defp to_range(op1, version1, :~>, version2) do
    to_range(:~>, version2, op1, version1)
  end

  defp to_range(:~>, version1, op2, version2) do
    range1 = to_range(:~>, version1)
    range2 = to_range(op2, version2)

    range = Range.intersect(range1, range2)

    unless Range.valid?(range) and Range.allows_any?(range1, range2) do
      throw({__MODULE__, :invalid_constraint})
    end

    range
  end

  defp to_range(op1, version1, op2, version2)
       when op1 in @allowed_range_ops and op2 in @allowed_range_ops do
    range =
      Map.merge(to_range(op1, version1), to_range(op2, version2), fn
        :__struct__, Range, Range -> Range
        :min, nil, value -> value
        :min, value, nil -> value
        :max, nil, value -> value
        :max, value, nil -> value
        :include_min, value, value -> value
        :include_min, false, value -> value
        :include_min, value, false -> value
        :include_max, value, value -> value
        :include_max, false, value -> value
        :include_max, value, false -> value
      end)

    unless Range.valid?(range) do
      throw({__MODULE__, :invalid_constraint})
    end

    range
  end

  defp to_version({major, minor, patch, pre, _build}),
    do: %Elixir.Version{major: major, minor: minor, patch: patch, pre: pre}

  # Vendored from https://github.com/elixir-lang/elixir/blob/0ff6522/lib/elixir/lib/version.ex#L495
  defmodule Parser do
    @moduledoc false

    operators = [
      {">=", :>=},
      {"<=", :<=},
      {"~>", :~>},
      {">", :>},
      {"<", :<},
      {"==", :==},
      {" or ", :or},
      {" and ", :and}
    ]

    def parse(string) do
      revert_lexed(lexer(string), [])
    end

    defp lexer(string) do
      lexer(string, "", [])
    end

    for {string_op, atom_op} <- operators do
      defp lexer(unquote(string_op) <> rest, buffer, acc) do
        lexer(rest, "", [unquote(atom_op) | maybe_prepend_buffer(buffer, acc)])
      end
    end

    defp lexer(" " <> rest, buffer, acc) do
      lexer(rest, "", maybe_prepend_buffer(buffer, acc))
    end

    defp lexer(<<char::utf8, rest::binary>>, buffer, acc) do
      lexer(rest, <<buffer::binary, char::utf8>>, acc)
    end

    defp lexer(<<>>, buffer, acc) do
      maybe_prepend_buffer(buffer, acc)
    end

    defp maybe_prepend_buffer("", acc), do: acc

    defp maybe_prepend_buffer(buffer, [head | _] = acc)
         when is_atom(head) and head not in [:and, :or],
         do: [buffer | acc]

    defp maybe_prepend_buffer(buffer, acc),
      do: [buffer, :== | acc]

    defp revert_lexed([version, op, cond | rest], acc)
         when is_binary(version) and is_atom(op) and cond in [:or, :and] do
      with {:ok, version} <- validate_requirement(op, version) do
        revert_lexed(rest, [cond, op, version | acc])
      end
    end

    defp revert_lexed([version, op], acc) when is_binary(version) and is_atom(op) do
      with {:ok, version} <- validate_requirement(op, version) do
        {:ok, [op, version | acc]}
      end
    end

    defp revert_lexed(_rest, _acc), do: :error

    defp validate_requirement(op, version) do
      case parse_version(version, true) do
        {:ok, version} when op == :~> -> {:ok, version}
        {:ok, {_, _, patch, _, _} = version} when is_integer(patch) -> {:ok, version}
        _ -> :error
      end
    end

    defp parse_version(string, approximate?) when is_binary(string) do
      destructure [version_with_pre, build], String.split(string, "+", parts: 2)
      destructure [version, pre], String.split(version_with_pre, "-", parts: 2)
      destructure [major, minor, patch, next], String.split(version, ".")

      with nil <- next,
           {:ok, major} <- require_digits(major),
           {:ok, minor} <- require_digits(minor),
           {:ok, patch} <- maybe_patch(patch, approximate?),
           {:ok, pre_parts} <- optional_dot_separated(pre),
           {:ok, pre_parts} <- convert_parts_to_integer(pre_parts, []),
           {:ok, build_parts} <- optional_dot_separated(build) do
        {:ok, {major, minor, patch, pre_parts, build_parts}}
      else
        _other -> :error
      end
    end

    defp require_digits(nil), do: :error

    defp require_digits(string) do
      if leading_zero?(string), do: :error, else: parse_digits(string, "")
    end

    defp leading_zero?(<<?0, _, _::binary>>), do: true
    defp leading_zero?(_), do: false

    defp parse_digits(<<char, rest::binary>>, acc) when char in ?0..?9,
      do: parse_digits(rest, <<acc::binary, char>>)

    defp parse_digits(<<>>, acc) when byte_size(acc) > 0, do: {:ok, String.to_integer(acc)}
    defp parse_digits(_, _acc), do: :error

    defp maybe_patch(patch, approximate?)
    defp maybe_patch(nil, true), do: {:ok, nil}
    defp maybe_patch(patch, _), do: require_digits(patch)

    defp optional_dot_separated(nil), do: {:ok, []}

    defp optional_dot_separated(string) do
      parts = String.split(string, ".")

      if Enum.all?(parts, &(&1 != "" and valid_identifier?(&1))) do
        {:ok, parts}
      else
        :error
      end
    end

    defp convert_parts_to_integer([part | rest], acc) do
      case parse_digits(part, "") do
        {:ok, integer} ->
          if leading_zero?(part) do
            :error
          else
            convert_parts_to_integer(rest, [integer | acc])
          end

        :error ->
          convert_parts_to_integer(rest, [part | acc])
      end
    end

    defp convert_parts_to_integer([], acc) do
      {:ok, Enum.reverse(acc)}
    end

    defp valid_identifier?(<<char, rest::binary>>)
         when char in ?0..?9
         when char in ?a..?z
         when char in ?A..?Z
         when char == ?- do
      valid_identifier?(rest)
    end

    defp valid_identifier?(<<>>) do
      true
    end

    defp valid_identifier?(_other) do
      false
    end
  end
end
