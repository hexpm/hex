defmodule Explex do
  defrecord Package, [:name, :version, :deps]

  defrecord State, [:activated, :pending]
  defrecord Request, [:name, :req, :parent]
  defrecord Active, [:name, :version, :state, :parents, :possibles]

  @ets_table :explex_registry

  def start do
    opts = [
      :bag,
      { :keypos, Package.__record__(:index, :name) + 1 },
      :named_table,
      :public ]
    :ets.new(@ets_table, opts)
  end

  def stop do
    :ets.delete(@ets_table)
  end

  def add_packages(packages) do
    :ets.insert(@ets_table, packages)
  end

  def resolve(requests, locked // []) do
    { activated, pending } =
      Enum.reduce(locked, { HashDict.new, [] }, fn { name, version }, { dict, pending } ->
        active = Active[name: name, version: version, parents: [], possibles: []]
        dict = Dict.put(dict, name, active)
        pending = pending ++ get_deps(name, version)
        { dict, pending }
      end)

    requests =
      Enum.map(requests, fn { name, req } ->
        Request[name: name, req: req]
      end)

    do_resolve(activated, pending ++ requests)
  end

  defp do_resolve(activated, []) do
    Enum.map(activated, fn { name, Active[] = active } ->
      { name, active.version }
    end) |> Enum.reverse
  end

  defp do_resolve(activated, [request|pending]) do
    active = activated[request.name]

    if active do
      possibles = Enum.filter(active.possibles, &version_match?(&1, request.req))
      active = active.possibles(possibles).parents(wrap(request.parent) ++ active.parents)

      if version_match?(active.version, request.req) do
        activated = Dict.put(activated, request.name, active)
        do_resolve(activated, pending)
      else
        backtrack(active, activated)
      end
    else
      versions = get_versions(request.name)
        |> Enum.filter(&version_match?(&1, request.req))

      case versions do
        [] ->
          backtrack(activated[request.parent], activated)

        [version|possibles] ->
          new_pending = get_deps(request.name, version)
          state = State[activated: activated, pending: pending]
          new_active = Active[name: request.name, version: version, state: state,
                              possibles: possibles, parents: wrap(request.parent)]
          activated = Dict.put(activated, request.name, new_active)

          do_resolve(activated, pending ++ new_pending)
      end
    end
  end

  defp backtrack(nil, _activated) do
    nil
  end

  defp backtrack(Active[state: state] = active, activated) do
    case active.possibles do
      [] ->
        Enum.find_value(active.parents, fn parent ->
          backtrack(activated[parent], activated)
        end)

      [version|possibles] ->
        active = active.possibles(possibles).version(version)
        pending = get_deps(active.name, version)

        activated = Dict.put(state.activated, active.name, active)
        do_resolve(activated, state.pending ++ pending)
    end
  end

  defp version_match?(_version, nil), do: true
  defp version_match?(version, req), do: Version.match?(version, req)

  defp get_versions(package) do
    package_vsn_ix = Package.__record__(:index, :version) + 1
    :ets.lookup_element(@ets_table, package, package_vsn_ix)
      |> Enum.sort(&Version.gt?/2)
  end

  defp get_deps(package, version) do
    ms = [{
      Package[name: package, version: version, deps: :"$1"],
      [],
      [:"$1"] }]
    deps = :ets.select(@ets_table, ms) |> Enum.first

    Enum.map(deps, fn { name, req } ->
      Request[name: name, req: req, parent: package]
    end)
  end

  defp wrap(nil), do: []
  defp wrap(arg), do: [arg]
end
