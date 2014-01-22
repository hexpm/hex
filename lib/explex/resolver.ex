defmodule Explex.Resolver do
  defrecord State, [:activated, :pending]
  defrecord Request, [:name, :req, :parent]
  defrecord Active, [:name, :version, :state, :parents, :possibles]

  alias Explex.Registry
  alias Explex.Registry.Package

  def resolve(requests, locked // []) do
    { activated, pending } =
      Enum.reduce(locked, { HashDict.new, [] }, fn { name, version }, { dict, pending } ->
        if (versions = Registry.get_versions(name)) && version in versions do
          active = Active[name: name, version: version, parents: [], possibles: []]
          dict = Dict.put(dict, name, active)
          pending = pending ++ get_deps(name, version)
        end
        { dict, pending }
      end)

    requests =
      Enum.map(requests, fn { name, req } ->
        Request[name: name, req: req]
      end)

    do_resolve(activated, pending ++ requests)
  end

  defp do_resolve(activated, []) do
    Enum.map(activated, fn { name, Active[version: version] } ->
      { name, version }
    end) |> Enum.reverse
  end

  defp do_resolve(activated, [request|pending]) do
    active = activated[request.name]

    if active do
      possibles = Enum.filter(active.possibles, &vsn_match?(&1, request.req))
      active = active.possibles(possibles).parents(wrap(request.parent) ++ active.parents)

      if vsn_match?(active.version, request.req) do
        activated = Dict.put(activated, request.name, active)
        do_resolve(activated, pending)
      else
        backtrack(active, activated)
      end
    else
      versions = Registry.get_versions(request.name)
        |> Enum.filter(&vsn_match?(&1, request.req))

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

  def get_deps(package, version) do
    Package[deps: deps] = Registry.get_package(package, version)

    Enum.map(deps, fn { name, req } ->
      Request[name: name, req: req, parent: package]
    end)
  end

  defp vsn_match?(_version, nil),
    do: true
  defp vsn_match?(version, req) when is_regex(req),
    do: version =~ req
  defp vsn_match?(version, req) when is_binary(req),
    do: Version.match?(version, req)

  defp wrap(nil), do: []
  defp wrap(arg), do: [arg]
end
