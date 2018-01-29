defmodule Hex.Resolver.Backtracks do
  require Record

  Record.defrecordp(:parent, [:name, :repo, :version, :requirement, :repo_requirement])

  @ets :hex_backtracks

  def start() do
    :ets.new(@ets, [])
  end

  def stop(ets) do
    :ets.delete(ets)
  end

  def add(ets, repo, name, version, parents) do
    parents = order_parents(parents)

    case :ets.lookup(ets, {repo, name, parents}) do
      [{_, versions}] ->
        unless version in versions do
          :ets.insert(ets, {{repo, name, parents}, [version | versions]})
        end

      [] ->
        :ets.insert(ets, {{repo, name, parents}, [version]})
    end
  end

  def collect(ets) do
    :ets.tab2list(ets)
    |> normalize()
    |> merge_versions()
    |> expand_merged()
    |> sort_backtracks()
  end

  defp order_parents(parents) do
    parents
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp normalize(backtracks) do
    # Expand versions into individual backtracks again
    Enum.flat_map(backtracks, fn
      {{repo, name, parents}, []} ->
        [{{repo, name}, nil, parents}]

      {{repo, name, parents}, versions} ->
        Enum.map(versions, &{{repo, name}, &1, parents})
    end)
  end

  defp sort_backtracks(backtracks) do
    Enum.map(backtracks, fn {repo, name, versions, parents} ->
      # If nil is in versions it means any version of the dependency
      # will fail with the given parents. nil is from when the resolver
      # failed to activate ANY version of the dependency.
      versions = if nil in versions, do: [], else: Enum.sort(versions, &version_cmp/2)
      parents = sort_parents(parents)
      {name, versions, repo, parents}
    end)
    |> Enum.sort()
  end

  defp sort_parents(parents) do
    Enum.map(parents, fn parent(version: versions) = parent ->
      versions =
        versions
        |> List.wrap()
        |> Enum.reject(&is_nil/1)
        |> Enum.uniq()
        |> Enum.sort(&version_cmp/2)

      parent(parent, version: versions)
    end)
    |> Enum.sort()
  end

  defp expand_merged(merged) do
    Enum.flat_map(merged, fn {{repo, name}, list} ->
      Enum.map(list, fn {versions, parents} ->
        {repo, name, versions, parents}
      end)
    end)
  end

  defp merge_versions(backtracks) do
    # For each package try to merge the similar backtrack messages
    # from conflicts in the resolver.
    # Complete duplicates are skipped when inserting into the ets table.
    # If two backtracks have similar parents (only differing in the
    # parents versions) we merge them. Finally if a package has the exact
    # same conflicts for two different versions we also merge those.
    Enum.reduce(backtracks, %{}, fn {name, version, parents}, map ->
      parents = Enum.sort(parents)

      case Map.fetch(map, name) do
        {:ok, list} ->
          case merge_package_version(list, version, parents) do
            {:ok, list} ->
              Map.put(map, name, list)

            :error ->
              Map.put(map, name, [{[version], parents} | list])
          end

        :error ->
          Map.put(map, name, [{[version], parents}])
      end
    end)
  end

  # For a given a package try to merge this version and parents
  # into the existing ones
  defp merge_package_version(list, version, parents) do
    update_one(list, [], fn {versions, existing_parents} ->
      case merge_parents(existing_parents, parents, []) do
        {:ok, parents} ->
          {:ok, {insert_new(versions, version), parents}}

        :error ->
          :error
      end
    end)
  end

  # If two lists of parents match (in package name and requirement)
  # merge them and append the versions
  defp merge_parents([], [], acc), do: {:ok, Enum.reverse(acc)}

  defp merge_parents(
         [parent(repo: repo, name: name, requirement: req, version: versions) | existing],
         [parent(repo: repo, name: name, requirement: req, version: version) | new],
         acc
       ) do
    versions = [version | List.wrap(versions)]
    parent = parent(repo: repo, name: name, requirement: req, version: versions)
    merge_parents(existing, new, [parent | acc])
  end

  defp merge_parents(_, _, _), do: :error

  # Update a single element in a list
  defp update_one([], _acc, _fun), do: :error

  defp update_one([head | tail], acc, fun) do
    case fun.(head) do
      {:ok, new} -> {:ok, [new | acc] ++ tail}
      :error -> update_one(tail, [head | acc], fun)
    end
  end

  defp insert_new(list, elem) do
    if elem in list, do: list, else: [elem | list]
  end

  def unzip(list), do: do_unzip(list, [])

  def do_unzip([head | tail], acc) do
    acc = inner_unzip(head, acc)
    do_unzip(tail, acc)
  end

  def do_unzip([], acc), do: acc

  defp inner_unzip([x | xs], [y | ys]), do: [[x | y] | inner_unzip(xs, ys)]
  defp inner_unzip([x | xs], []), do: [[x] | inner_unzip(xs, [])]
  defp inner_unzip([], []), do: []

  defp version_cmp(a, b), do: Hex.Version.compare(a, b) != :gt

  def message({name, versions, repo, parents}, registry) do
    if parent_messages = parent_messages(registry, parents, repo, name, versions) do
      IO.ANSI.format([
        [:underline, "Failed to use \"", name, "\""],
        versions_message(registry, repo, name, versions),
        [" because", single_parent_message(parents), :reset, "\n"],
        parent_messages
      ])
      |> IO.iodata_to_binary()
    end
  end

  defp single_parent_message(parents) when length(parents) < 2 do
    " there are no packages that matches the requirement"
  end

  defp single_parent_message(_parents) do
    ""
  end

  defp parent_messages(registry, parents, child_repo, child, child_versions) do
    {mix, parents} = partition_mix(parents)

    parent_reasons =
      Enum.map(parents, &{&1, parent_reason(registry, &1, child_repo, child, child_versions)})

    mix_reason = {mix, parent_reason(registry, mix, child_repo, child, child_versions)}
    messages = Enum.map(parent_reasons, &parent_message(&1, registry))
    all_reasons = [mix_reason | parent_reasons]

    unless all_green?(all_reasons) do
      messages =
        if mix do
          messages ++ [parent_message(mix_reason, registry)]
        else
          messages
        end

      Enum.map(messages, &["  ", &1, "\n"])
    end
  end

  defp parent_message(
         {parent(name: "mix.lock", version: [], requirement: req), {color, pre_failed?}},
         _registry
       ) do
    [
      [:bright, "mix.lock", :reset, " specifies ", color, requirement(req), :reset],
      pre_message(pre_failed?)
    ]
  end

  defp parent_message(
         {parent(name: "mix.exs", version: [], requirement: req), {color, pre_failed?}},
         _registry
       ) do
    [
      [:bright, "mix.exs", :reset, " specifies ", color, requirement(req), :reset],
      pre_message(pre_failed?)
    ]
  end

  defp parent_message(
         {parent(repo: repo, name: name, version: versions, requirement: req),
          {color, pre_failed?}},
         registry
       ) do
    [
      [:bright, name, versions_message(registry, repo, name, versions), :reset],
      [" requires ", color, requirement(req), :reset],
      pre_message(pre_failed?)
    ]
  end

  defp parent_message({nil, nil}, _registry), do: []

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

  defp parent_reason(_registry, nil, _child_repo, _child, _versions), do: nil

  defp parent_reason(registry, parent, child_repo, child, []) do
    versions = registry.versions(child_repo, child)
    parent_reason(registry, parent, child_repo, child, versions)
  end

  defp parent_reason(_registry, parent(requirement: req), _child_repo, _child, versions) do
    num_failures = Enum.count(versions, &(not version_match?(&1, req, [])))
    num_versions = length(versions)
    pre_failures = Enum.count(versions, &(not version_match?(&1, req, allow_pre: true)))
    pre_failed? = pre_failures < num_failures

    {parent_color(num_versions, num_failures), pre_failed?}
  end

  defp parent_color(_versions, 0), do: :green
  defp parent_color(versions, versions), do: :red
  defp parent_color(versions, failures) when failures < versions, do: :yellow

  defp version_match?(_version, nil, _opts) do
    true
  end

  defp version_match?(version, req, opts) do
    Hex.Version.match?(version, req, opts)
  end

  # Try converting lists of versions to a version range if the list is
  # a complete range of versions of the package.
  # For example ["0.1.0", "0.1.1", "0.2.0", "0.3.0"] can be converted
  # to "to 0.1.0 from 0.3.0" if there are no other releases of the package
  # between 0.1.0 and 0.3.0.
  defp versions_message(registry, repo, package, versions) do
    case {versions, merge_versions?(registry, repo, package, versions)} do
      {[], _} ->
        ""

      {[x], _} ->
        [" (version ", to_string(x), ")"]

      {[x, y], _} ->
        [" (versions ", to_string(x), " and ", to_string(y), ")"]

      {_, true} when length(versions) > 2 ->
        first = versions |> List.first() |> to_string()
        last = versions |> List.last() |> to_string()
        [" (versions ", first, " to ", last, ")"]

      _ ->
        versions = Enum.map(versions, &to_string/1)
        [" (versions ", Enum.join(versions, ", "), ")"]
    end
  end

  defp merge_versions?(_registry, _repo, _package, []) do
    false
  end

  defp merge_versions?(registry, repo, package, versions) do
    all_versions = registry.versions(repo, package)
    sub_range?(all_versions, versions)
  end

  # Assumes lists are sorted
  defp sub_range?(outer, inner), do: do_sub_range?(outer, inner, false)

  defp do_sub_range?(_, [], _), do: true
  defp do_sub_range?([], _, _), do: false
  defp do_sub_range?([x | outer], [x | inner], _), do: do_sub_range?(outer, inner, true)
  defp do_sub_range?(_, _, true), do: false
  defp do_sub_range?([_ | outer], inner, false), do: do_sub_range?(outer, inner, false)

  defp requirement(nil), do: ">= 0.0.0"
  defp requirement(req), do: req
end
