# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Incompatibility do
  @moduledoc false

  import Kernel, except: [to_string: 1]
  alias Hex.Solver.{Constraint, Incompatibility, PackageRange, Term}
  alias Hex.Solver.Constraints.Range

  defstruct terms: [], cause: nil

  # Causes:
  # * {:conflict, incompatibility, cause}
  # * :root
  # * :dependency
  # * :no_versions
  # * :package_not_found

  def new(terms, cause) do
    terms =
      if length(terms) != 1 and match?({:conflict, _, _}, cause) and
           Enum.any?(terms, &(&1.positive or &1.package_range.name == "$root")) do
        Enum.filter(terms, &(not &1.positive or &1.package_range.name != "$root"))
      else
        terms
      end

    case terms do
      [_first] ->
        %Incompatibility{terms: terms, cause: cause}

      [%{package_range: %{name: first}}, %{package_range: %{name: second}}]
      when first != second ->
        %Incompatibility{terms: terms, cause: cause}

      _ ->
        terms =
          Enum.reduce(terms, %{}, fn term, map ->
            Map.update(map, term.package_range.name, term, &Term.intersect(&1, term))
          end)

        %Incompatibility{terms: Map.values(terms), cause: cause}
    end
  end

  def failure?(%Incompatibility{terms: []}), do: true
  def failure?(%Incompatibility{terms: [%{package_range: %{name: "$root"}}]}), do: true
  def failure?(%Incompatibility{}), do: false

  def to_string(incompatibilty, opts \\ [])

  def to_string(
        %Incompatibility{
          cause: :dependency,
          terms: [
            %Term{package_range: %PackageRange{name: "$lock"}, positive: true},
            %Term{positive: false} = dependee
          ]
        },
        opts
      ) do
    "\"the lock\" specifies #{bright_term_abs(dependee, opts)}"
  end

  def to_string(
        %Incompatibility{
          cause: :dependency,
          terms: [%Term{positive: true} = depender, %Term{positive: false} = dependee]
        },
        opts
      ) do
    "#{terse_every(depender, opts)} depends on #{bright_term_abs(dependee, opts)}"
  end

  def to_string(%Incompatibility{cause: :no_versions, terms: terms}, opts) do
    [%Term{positive: true} = term] = terms

    "no versions of #{package_name(term, opts)} match #{bright(term.package_range.constraint, opts)}"
  end

  def to_string(%Incompatibility{cause: :package_not_found, terms: terms}, opts) do
    [%Term{positive: true} = term] = terms
    "#{package_name(term, opts)} doesn't exist"
  end

  def to_string(%Incompatibility{cause: :root, terms: terms}, opts) do
    [%Term{positive: false} = term] = terms
    "#{package_name(term, opts)} is #{bright(term.package_range.constraint, opts)}"
  end

  def to_string(%Incompatibility{terms: []}, _opts) do
    "version solving failed"
  end

  def to_string(
        %Incompatibility{terms: [%Term{package_range: %PackageRange{name: "$root"}}]},
        _opts
      ) do
    "version solving failed"
  end

  def to_string(
        %Incompatibility{
          terms: [
            %Term{
              positive: true,
              package_range: %PackageRange{constraint: %Range{min: nil, max: nil}}
            } = term
          ]
        },
        opts
      ) do
    "no version of #{package_name(term, opts)} is allowed"
  end

  def to_string(%Incompatibility{terms: [%Term{positive: true} = term]}, opts) do
    "#{terse_name(term, opts)} is forbidden"
  end

  def to_string(%Incompatibility{terms: [%Term{positive: false} = term]}, opts) do
    "#{terse_name(%Term{term | positive: true}, opts)} is required"
  end

  def to_string(
        %Incompatibility{terms: [%{positive: true} = left, %{positive: true} = right]},
        opts
      ) do
    "#{terse_name(left, opts)} is incompatible with #{terse_name(right, opts)}"
  end

  def to_string(
        %Incompatibility{terms: [%{positive: false} = left, %{positive: false} = right]},
        opts
      ) do
    "either #{bright_term_abs(left, opts)} or #{bright_term_abs(right, opts)}"
  end

  def to_string(%Incompatibility{terms: terms}, opts) do
    {positive, negative} = Enum.split_with(terms, & &1.positive)

    cond do
      positive != [] and negative != [] ->
        case positive do
          [term] ->
            "#{bright_term_abs(term, opts)} requires #{Enum.map_join(negative, " or ", &bright_term_abs(&1, opts))}"

          _ ->
            "if #{Enum.map_join(positive, " and ", &bright_term_abs(&1, opts))} then #{Enum.map_join(negative, " or ", &bright_term_abs(&1, opts))}"
        end

      positive != [] ->
        "one of #{Enum.map_join(positive, " or ", &bright_term_abs(&1, opts))} must be false"

      negative != [] ->
        "one of #{Enum.map_join(negative, " or ", &bright_term_abs(&1, opts))} must be true"
    end
  end

  def to_string_and(left, right, opts \\ [])

  def to_string_and(
        %Incompatibility{
          terms: [%{package_range: %{name: "$lock"}}, _dependency],
          cause: :dependency
        } = left,
        %Incompatibility{
          terms: [%{package_range: %{name: "$root"}}, %{package_range: %{name: "$lock"}}],
          cause: :dependency
        },
        opts
      ) do
    to_string(left, opts)
  end

  def to_string_and(%Incompatibility{} = left, %Incompatibility{} = right, opts) do
    cond do
      requires_both = try_requires_both(left, right, opts) ->
        requires_both

      requires_through = try_requires_through(left, right, opts) ->
        requires_through

      requires_forbidden = try_requires_forbidden(left, right, opts) ->
        requires_forbidden

      true ->
        [
          to_string(left, opts),
          maybe_line(opts[:left_line]),
          " and ",
          to_string(right, opts),
          maybe_line(opts[:right_line])
        ]
    end
    |> IO.chardata_to_string()
  end

  defp try_requires_both(left, right, opts) do
    if length(left.terms) == 1 or length(right.terms) == 1 do
      throw({__MODULE__, :try_requires_both})
    end

    left_positive = single_term(left, & &1.positive)
    right_positive = single_term(right, & &1.positive)

    if !left_positive || !right_positive ||
         left_positive.package_range != right_positive.package_range do
      throw({__MODULE__, :try_requires_both})
    end

    left_negatives =
      left.terms
      |> Enum.reject(& &1.positive)
      |> Enum.map_join(" or ", &bright_term_abs(&1, opts))

    right_negatives =
      right.terms
      |> Enum.reject(& &1.positive)
      |> Enum.map_join(" or ", &bright_term_abs(&1, opts))

    dependency? = left.cause == :dependency and right.cause == :dependency

    [
      terse_every(left_positive, opts),
      " ",
      cause_verb(dependency?),
      " both ",
      left_negatives,
      maybe_line(opts[:left_line]),
      " and ",
      right_negatives,
      maybe_line(opts[:right_line])
    ]
  catch
    {__MODULE__, :try_requires_both} -> nil
  end

  defp try_requires_through(right, left, opts) do
    if length(left.terms) == 1 or length(right.terms) == 1 do
      throw({__MODULE__, :try_requires_through})
    end

    left_negative = single_term(left, &(not &1.positive))
    right_negative = single_term(right, &(not &1.positive))
    left_positive = single_term(left, & &1.positive)
    right_positive = single_term(right, & &1.positive)

    if !left_negative && !right_negative do
      throw({__MODULE__, :try_requires_through})
    end

    {prior, prior_negative, prior_line, latter, latter_line} =
      cond do
        left_negative && right_positive &&
          left_negative.package_range.name == right_positive.package_range.name &&
            Term.satisfies?(Term.inverse(left_negative), right_positive) ->
          {left, left_negative, opts[:left_line], right, opts[:right_line]}

        right_negative && left_positive &&
          right_negative.package_range.name == left_positive.package_range.name &&
            Term.satisfies?(Term.inverse(right_negative), left_positive) ->
          {right, right_negative, opts[:right_line], left, opts[:left_line]}

        true ->
          throw({__MODULE__, :try_requires_through})
      end

    prior_positives = Enum.filter(prior.terms, & &1.positive)

    buffer =
      if length(prior_positives) > 1 do
        prior_string = Enum.map_join(prior_positives, " or ", &bright_term_abs(&1, opts))
        "if #{prior_string} then "
      else
        "#{terse_every(List.first(prior_positives), opts)} #{cause_verb(prior)} "
      end

    buffer = [
      buffer,
      bright_term_abs(prior_negative, opts),
      maybe_line(prior_line),
      " which ",
      cause_verb(latter)
    ]

    latter_string =
      latter.terms
      |> Enum.reject(& &1.positive)
      |> Enum.map_join(" or ", &bright_term_abs(&1, opts))

    [buffer, " ", latter_string, maybe_line(latter_line)]
  catch
    {__MODULE__, :try_requires_through} -> nil
  end

  defp try_requires_forbidden(left, right, opts) do
    if length(left.terms) != 1 and length(right.terms) != 1 do
      throw({__MODULE__, :try_requires_forbidden})
    end

    {prior, prior_line, latter, latter_line} =
      if length(left.terms) == 1 do
        {right, opts[:right_line], left, opts[:left_line]}
      else
        {left, opts[:left_line], right, opts[:right_line]}
      end

    negative = single_term(prior, &(not &1.positive))

    unless negative do
      throw({__MODULE__, :try_requires_forbidden})
    end

    unless Term.satisfies?(Term.inverse(negative), List.first(latter.terms)) do
      throw({__MODULE__, :try_requires_forbidden})
    end

    positives = Enum.filter(prior.terms, & &1.positive)

    buffer =
      case positives do
        [positive] ->
          [terse_every(positive, opts), " ", cause_verb(prior), " "]

        _ ->
          ["if ", Enum.map_join(positives, " or ", &bright_term_abs(&1, opts)), " then "]
      end

    buffer = [
      buffer,
      bright_term_abs(List.first(latter.terms), opts),
      maybe_line(prior_line),
      " "
    ]

    buffer =
      case latter.cause do
        :no_versions -> [buffer, "which doesn't match any versions"]
        :package_not_found -> [buffer, "which doesn't exist"]
        _ -> [buffer, "which is forbidden"]
      end

    [buffer, maybe_line(latter_line)]
  catch
    {__MODULE__, :try_requires_forbidden} -> nil
  end

  defp bright(string, opts) do
    if Keyword.get(opts, :ansi, false) do
      [IO.ANSI.bright(), Kernel.to_string(string), IO.ANSI.reset()]
    else
      ~s("#{string}")
    end
  end

  defp cause_verb(true), do: "depends on"
  defp cause_verb(false), do: "requires"
  defp cause_verb(%Incompatibility{cause: :dependency}), do: "depends on"
  defp cause_verb(%Incompatibility{cause: _}), do: "requires"

  defp maybe_line(nil), do: ""
  defp maybe_line(line), do: " (#{line})"

  defp single_term(%Incompatibility{terms: terms}, fun) do
    Enum.reduce_while(terms, nil, fn term, found ->
      if fun.(term) do
        if found do
          {:halt, nil}
        else
          {:cont, term}
        end
      else
        {:cont, found}
      end
    end)
  end

  defp package_name(%Term{package_range: %PackageRange{name: "$root"}}, opts),
    do: bright("your app", opts)

  defp package_name(%Term{package_range: %PackageRange{name: "$lock"}}, opts),
    do: bright("the lock", opts)

  defp package_name(%Term{package_range: %PackageRange{repo: nil, name: name}}, opts),
    do: bright(name, opts)

  defp package_name(%Term{package_range: %PackageRange{repo: repo, name: name}}, opts),
    do: bright("#{repo}/#{name}", opts)

  defp terse_name(term, opts) do
    if Constraint.any?(term.package_range.constraint) do
      package_name(term, opts)
    else
      bright(PackageRange.to_string(term.package_range), opts)
    end
  end

  defp terse_every(%Term{package_range: %PackageRange{name: "$root"}}, opts),
    do: bright("your app", opts)

  defp terse_every(%Term{package_range: %PackageRange{name: "$lock"}}, opts),
    do: bright("the lock", opts)

  defp terse_every(term, opts) do
    if Constraint.any?(term.package_range.constraint) do
      "every version of #{package_name(term, opts)}"
    else
      bright(PackageRange.to_string(term.package_range), opts)
    end
  end

  defp term_abs(term), do: %Term{term | positive: true}

  defp bright_term_abs(term, opts), do: bright(term_abs(term), opts)

  defimpl String.Chars do
    defdelegate to_string(incompatibility), to: Hex.Solver.Incompatibility
  end

  defimpl Inspect do
    def inspect(%{terms: terms, cause: cause}, _opts) do
      "#Incompatibility<#{Enum.map_join(terms, ", ", &Kernel.inspect/1)}#{maybe(", cause: ", cause)}>"
    end

    defp maybe(_prefix, nil), do: ""
    defp maybe(prefix, value), do: "#{prefix}#{inspect(value)}"
  end
end
