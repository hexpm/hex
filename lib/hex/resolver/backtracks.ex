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
    parents =
      parents
      |> Enum.uniq
      |> Enum.sort

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
    |> Enum.sort
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
    |> Enum.sort
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

  def message({name, versions, parents}) do
    if parent_messages = parent_messages(parents, name, versions) do
      IO.ANSI.format([
        :underline, "Failed to use \"", name, "\"", versions_message(name, versions),
        " because", :reset, "\n", parent_messages
      ])
      |> IO.iodata_to_binary
    end
  end

  defp parent_messages(parents, child, child_versions) do
    {mix, parents} = partition_mix(parents)
    parent_reasons = Enum.map(parents, &{&1, parent_reason(&1, child, child_versions)})
    mix_reason = {mix, parent_reason(mix, child, child_versions)}
    messages = Enum.map(parent_reasons, &parent_message/1)
    all_reasons = [mix_reason|parent_reasons]

    unless all_green?(all_reasons) do
      messages =
        if mix,
          do: messages ++ [parent_message(mix_reason)],
        else: messages

      Enum.map(messages, &["  ", &1, "\n"])
    end
  end

  defp parent_message({parent(name: "mix.lock", version: [], requirement: req), {color, pre_failed?}}) do
    [:bright, "mix.lock", :reset, " specifies ", color, requirement(req), :reset, pre_message(pre_failed?)]
  end

  defp parent_message({parent(name: "mix.exs", version: [], requirement: req), {color, pre_failed?}}) do
    [:bright, "mix.exs", :reset, " specifies ", color, requirement(req), :reset, pre_message(pre_failed?)]
  end

  defp parent_message({parent(name: name, version: versions, requirement: req), {color, pre_failed?}}) do
    [:bright, name, versions_message(name, versions), :reset, " requires ", color,
     requirement(req), :reset, pre_message(pre_failed?)]
  end

  defp parent_message({nil, nil}), do: []

  defp pre_message(true), do: " *"
  defp pre_message(false), do: ""

  defp all_green?(reasons) do
    Enum.all?(reasons, fn
      {nil, nil} -> false
      {_parent, {color, _pre}} -> color == :green
    end)
  end

  defp partition_mix(parents) do
    map =
      Enum.into(parents, %{}, fn parent ->
        {parent(parent, :name), parent}
      end)

    parents =
      Enum.reject(parents, fn parent(name: name) ->
        name in ~w(mix.exs mix.lock)
      end)

    {map["mix.lock"] || map["mix.exs"], parents}
  end

  defp parent_reason(nil, _child, _versions), do: nil
  defp parent_reason(parent, child, []) do
    versions = Hex.Registry.versions(child)
    parent_reason(parent, child, versions)
  end
  defp parent_reason(parent(requirement: req), _child, versions) do
    num_failures = Enum.count(versions, &(not version_match?(&1, req, [])))
    num_versions = length(versions)
    pre_failures = Enum.count(versions, &(not version_match?(&1, req, allow_pre: true)))
    pre_failed?  = pre_failures < num_failures

    {parent_color(num_versions, num_failures), pre_failed?}
  end

  defp parent_color(_versions, 0), do: :green
  defp parent_color(versions, versions), do: :red
  defp parent_color(versions, failures) when failures < versions, do: :yellow

  defp version_match?(_version, nil, _opts),
    do: true
  defp version_match?(version, req, opts),
    do: Hex.Version.match?(version, req, opts)

  # Try converting lists of versions to a version range if the list is
  # a complete range of versions of the package.
  # For example ["0.1.0", "0.1.1", "0.2.0", "0.3.0"] can be converted
  # to "to 0.1.0 from 0.3.0" if there are no other releases of the package
  # between 0.1.0 and 0.3.0.
  defp versions_message(package, versions) do
    case {versions, merge_versions?(package, versions)} do
      {[], _} ->
        ""
      {[x], _} ->
        [" (version ", to_string(x), ")"]
      {[x, y], _} ->
        [" (versions ", to_string(x), " and ", to_string(y), ")"]
      {_, true} when length(versions) > 2 ->
        first = versions |> List.first |> to_string
        last = versions |> List.last |> to_string
        [" (versions ", first, " to ", last, ")"]
      _ ->
        versions = Enum.map(versions, &to_string/1)
        [" (versions ", Enum.join(versions, ", "), ")"]
    end
  end

  defp merge_versions?(_package, []), do: false
  defp merge_versions?(package, versions) do
    all_versions = Hex.Registry.versions(package)
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
  defp requirement(req), do: req
end
