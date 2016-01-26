defmodule Hex.Resolver.Backtracks do
  require Record

  Record.defrecordp :parent, [:name, :version, :requirement]

  @ets :hex_backtracks

  def start do
    :ets.new(@ets, [:named_table])
  end

  def stop do
    :ets.delete(@ets)
  end

  def add(name, version, parents) do
    parents = Enum.sort(parents, &parent_cmp/2)

    case :ets.lookup(@ets, {name, parents}) do
      [{_, versions}] ->
        unless version in versions do
          :ets.insert(@ets, {{name, parents}, [version|versions]})
        end
      [] ->
        :ets.insert(@ets, {{name, parents}, [version]})
    end
  end

  def collect do
    :ets.tab2list(@ets)
    |> normalize
    |> merge_versions
    |> sort_backtracks
  end

  defp normalize(backtracks) do
    # Expand versions into individual backtracks again
    Enum.flat_map(backtracks, fn
      {{name, parents}, []} ->
        [{name, nil, parents}]
      {{name, parents}, versions} ->
        Enum.map(versions, &{name, &1, parents})
    end)
  end

  defp sort_backtracks(backtracks) do
    Enum.map(backtracks, fn {name, versions, parents} ->
      # If nil is in versions it means any version of the dependency
      # will fail with the given parents. nil is from when the resolver
      # failed to activate ANY version of the dependency.
      versions = if nil in versions, do: [], else: Enum.sort(versions, &version_cmp/2)
      parents = sort_parents(parents)
      {name, versions, parents}
    end)
    |> Enum.sort()
  end

  defp sort_parents(parents) do
    Enum.map(parents, fn parent(version: versions) = parent ->
      versions =
        versions
        |> List.wrap
        |> Enum.reject(&is_nil/1)
        |> Enum.uniq
        |> Enum.sort(&version_cmp/2)
      parent(parent, version: versions)
    end)
    |> Enum.sort(&parent_cmp/2)
  end

  defp merge_versions(backtracks) do
    merged = do_merge_versions(backtracks)

    Enum.flat_map(merged, fn {name, list} ->
      Enum.map(list, fn {versions, parents} ->
        {name, versions, parents}
      end)
    end)
  end

  defp do_merge_versions(backtracks) do
    # For each package try to merge the merge similar backtrack messages
    # from conflicts in the resolver.
    # Complete duplicates are skipped when inserting into the ets table.
    # If two backtracks have similar parents (only differing in the
    # parents versions) we merge them. Finally if a package has the exact
    # same conflicts for two different versions we also merge those.
    Enum.reduce(backtracks, %{}, fn {name, version, parents}, map ->
      parents = Enum.sort(parents)

      case Map.fetch(map, name) do
        {:ok, list} ->
          case try_merge_package_version(list, version, parents) do
            {:ok, list} ->
              Map.put(map, name, list)
            :error ->
              Map.put(map, name, [{[version], parents}|list])
          end

        :error ->
          Map.put(map, name, [{[version], parents}])
      end
    end)
  end

  # For a given a package try to merge this version and parents
  # into the existing ones
  defp try_merge_package_version(list, version, parents) do
    update_one(list, [], fn {versions, existing_parents} ->
      case try_merge_parents(existing_parents, parents, []) do
        {:ok, parents} ->
          {:ok, {insert_new(versions, version), parents}}
        :error ->
          :error
      end
    end)
  end

  # If two lists of parents match (in package name and requirement)
  # merge them and append the versions
  defp try_merge_parents([], [], acc), do: {:ok, Enum.reverse(acc)}
  defp try_merge_parents([parent(name: name, requirement: req, version: versions)|existing],
                         [parent(name: name, requirement: req, version: version)|new],
                         acc) do
    versions = [version|List.wrap(versions)]
    parent = parent(name: name, requirement: req, version: versions)
    try_merge_parents(existing, new, [parent|acc])
  end
  defp try_merge_parents(_, _, _), do: :error

  # Update a single element in a list
  defp update_one([], _acc, _fun), do: :error
  defp update_one([head|tail], acc, fun) do
    case fun.(head) do
      {:ok, new} -> {:ok, [new|acc] ++ tail}
      :error     -> update_one(tail, [head|acc], fun)
    end
  end

  defp insert_new(list, elem) do
    if elem in list, do: list, else: [elem|list]
  end

  def unzip(list), do: do_unzip(list, [])

  def do_unzip([head|tail], acc) do
    acc = inner_unzip(head, acc)
    do_unzip(tail, acc)
  end
  def do_unzip([], acc), do: acc

  defp inner_unzip([x|xs], [y|ys]), do: [[x|y]|inner_unzip(xs, ys)]
  defp inner_unzip([x|xs], []), do: [[x]|inner_unzip(xs, [])]
  defp inner_unzip([], []), do: []

  defp version_cmp(a, b), do: Hex.Version.compare(a, b) != :gt

  # TODO: Handle sorting of mix.exs from umbrellas
  defp parent_cmp(parent(name: "mix.exs"), _),  do: true
  defp parent_cmp(_, parent(name: "mix.exs")),  do: false
  defp parent_cmp(parent(name: "mix.lock"), _), do: true
  defp parent_cmp(_, parent(name: "mix.lock")), do: false
  defp parent_cmp(parent1, parent2),            do: parent1 <= parent2

  def message({name, versions, parents}) do
    "Conflict on #{name}#{versions_message(name, versions)}\n" <>
    "  " <> Enum.map_join(parents, "\n  ", &parent_message/1)
  end

  defp parent_message(parent(name: name, version: versions, requirement: req)) do
    "#{name}#{versions_message(name, versions)}: #{requirement(req)}"
  end

  # Try converting lists of versions to a version range if the list is
  # a complete range of versions of the package.
  # For example ["0.1.0", "0.1.1", "0.2.0", "0.3.0"] can be converted
  # to "to 0.1.0 from 0.3.0" if there are no other releases of the package
  # between 0.1.0 and 0.3.0.
  defp versions_message(package, versions) do
    case {length(versions), merge_versions?(package, versions)} do
      {0, _} ->
        ""
      {x, true} when x > 2 ->
        " from #{List.first(versions)} to #{List.last(versions)}"
      _ ->
        " " <> Enum.join(versions, ", ")
    end
  end

  defp merge_versions?(_package, []), do: false
  defp merge_versions?(package, versions) do
    all_versions = Hex.Registry.get_versions(package)
    sub_range?(all_versions, versions)
  end

  # Assumes lists are sorted
  defp sub_range?(outer, inner), do: do_sub_range?(outer, inner, false)

  defp do_sub_range?(_, [], _),                do: true
  defp do_sub_range?([], _, _),                do: false
  defp do_sub_range?([x|outer], [x|inner], _), do: do_sub_range?(outer, inner, true)
  defp do_sub_range?(_, _, true),              do: false
  defp do_sub_range?([_|outer], inner, false), do: do_sub_range?(outer, inner, false)

  defp requirement(nil), do: ">= 0.0.0"
  defp requirement(req), do: req.source
end
