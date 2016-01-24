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
    new_versions = if version, do: [version], else: []

    case :ets.lookup(@ets, {name, parents}) do
      [{_, old_versions}] ->
        unless is_nil(version) or version in old_versions do
          :ets.insert(@ets, {{name, parents}, new_versions ++ old_versions})
        end
      [] ->
        :ets.insert(@ets, {{name, parents}, new_versions})
    end
  end

  def collect do
    :ets.tab2list(@ets)
    |> normalize
    |> merge_similar_parents
    |> sort_backtracks
  end

  defp normalize(backtracks) do
    Enum.map(backtracks, fn {{name, parents}, versions} ->
      {name, versions, parents}
    end)
  end

  defp sort_backtracks(backtracks) do
    Enum.map(backtracks, fn {name, versions, parents} ->
      versions = Enum.sort(versions, &version_cmp/2)
      parents =
        Enum.map(parents, fn parent(version: versions) = parent ->
          parent(parent, version: Enum.sort(versions, &version_cmp/2))
        end)
        |> Enum.sort(&parent_cmp/2)
      {name, versions, parents}
    end)
    |> Enum.sort()
  end

  # Merge lists of parents that are identical except they differ only in
  # the version of a parent
  defp merge_similar_parents(backtracks) do
    # Collect all lists of parents for each unique pair of {name, version}
    # from conflicts
    backtracks =
      group_by(backtracks, fn {name, versions, parents} ->
        {{name, versions}, parents}
      end)

    Enum.flat_map(backtracks, fn {{name, versions}, parents} ->
      # Collect all lists of parents that have the same parents in the name
      # and requirement, so they only differ in the version of each parent
      parents_map =
        group_by(parents, fn parents ->
          Enum.reduce(parents, {[], []}, fn parent(name: name, requirement: req, version: version), {keys, versions} ->
            {[{name, req}|keys], [version|versions]}
          end)
        end)

      # Rebuild the parents by inserting the grouped versions to each parent
      Enum.map(parents_map, fn {keys, parent_versions} ->
        parent_versions = unzip(parent_versions, [])
        zipped = Enum.zip(keys, parent_versions)

        parents =
          Enum.map(zipped, fn {{name, req}, parent_versions} ->
            parent_versions = Enum.reject(parent_versions, &is_nil/1)
                              |> Enum.uniq
            parent(name: name, requirement: req, version: parent_versions)
          end)

        {name, versions, parents}
      end)
    end)
  end

  defp group_by(enum, fun) do
    Enum.reduce(enum, %{}, fn elem, map ->
      {key, value} = fun.(elem)
      Map.update(map, key, [value], &[value|&1])
    end)
  end

  def unzip([head|tail], acc) do
    acc = do_unzip(head, acc)
    unzip(tail, acc)
  end
  def unzip([], acc), do: acc

  defp do_unzip([x|xs], [y|ys]), do: [[x|y]|do_unzip(xs, ys)]
  defp do_unzip([x|xs], []), do: [[x]|do_unzip(xs, [])]
  defp do_unzip([], []), do: []

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
    sub_range?(all_versions, versions, false)
  end

  # Assumes lists are sorted
  defp sub_range?(_, [], _),                do: true
  defp sub_range?([], _, _),                do: false
  defp sub_range?([x|outer], [x|inner], _), do: sub_range?(outer, inner, true)
  defp sub_range?(_, _, true),              do: false
  defp sub_range?([_|outer], inner, false), do: sub_range?(outer, inner, false)

  defp requirement(nil), do: ">= 0.0.0"
  defp requirement(req), do: req.source
end
