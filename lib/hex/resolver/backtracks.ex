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
        # TODO: Check this
        unless is_nil(version) or version in old_versions do
          :ets.insert(@ets, {{name, parents}, new_versions ++ old_versions})
        end
      [] ->
        :ets.insert(@ets, {{name, parents}, new_versions})
    end
  end

  def collect do
    :ets.tab2list(@ets)
    |> sort_backtracks
  end

  defp sort_backtracks(backtracks) do
    backtracks
    |> Enum.map(fn {{name, parents}, versions} ->
         versions = Enum.sort(versions, &(Hex.Version.compare(&1, &2) != :lt))
         {name, versions, parents}
       end)
    |> Enum.sort()
  end

  # TODO: Handle sorting of mix.exs from umbrellas
  defp parent_cmp(parent(name: "mix.exs"), _),  do: true
  defp parent_cmp(_, parent(name: "mix.exs")),  do: false
  defp parent_cmp(parent(name: "mix.lock"), _), do: true
  defp parent_cmp(_, parent(name: "mix.lock")), do: false
  defp parent_cmp(parent1, parent2),            do: parent1 <= parent2
end
