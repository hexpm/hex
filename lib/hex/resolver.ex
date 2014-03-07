defmodule Hex.Resolver do
  defrecord State, [:activated, :pending]
  defrecord Request, [:name, :req, :parent]
  defrecord Active, [:name, :version, :state, :parents, :possibles]

  alias Hex.Registry

  def resolve(requests, locked \\ []) do
    { activated, pending } =
      Enum.reduce(locked, { HashDict.new, [] }, fn { name, version }, { dict, pending } ->
        if (versions = Registry.get_versions(name)) && version in versions do
          active = Active[name: name, version: version, parents: [], possibles: []]
          dict = Dict.put(dict, name, active)
          pending = pending ++ get_deps(name, version, [])
        end
        { dict, pending }
      end)

    overriden =
      Enum.flat_map(requests, fn
        { name, _req, true }   -> [name]
        { _name, _req, false } -> []
      end)

    requests =
      Enum.map(requests, fn { name, req, _override? } ->
        Request[name: name, req: req]
      end)

    do_resolve(activated, pending ++ requests, overriden)
  end

  defp do_resolve(activated, [], _overriden) do
    Enum.map(activated, fn { name, Active[version: version] } ->
      { name, version }
    end) |> Enum.reverse
  end

  defp do_resolve(activated, [request|pending], overriden) do
    active = activated[request.name]

    if active do
      possibles = Enum.filter(active.possibles, &vsn_match?(&1, request.req))
      active = active.possibles(possibles).parents(wrap(request.parent) ++ active.parents)

      if vsn_match?(active.version, request.req) do
        activated = Dict.put(activated, request.name, active)
        do_resolve(activated, pending, overriden)
      else
        backtrack(active, activated, overriden)
      end
    else
      versions = Registry.get_versions(request.name)
        |> Enum.filter(&vsn_match?(&1, request.req))
        |> Enum.reverse

      case versions do
        [] ->
          backtrack(activated[request.parent], activated, overriden)

        [version|possibles] ->
          new_pending = get_deps(request.name, version, overriden)
          state = State[activated: activated, pending: pending]
          new_active = Active[name: request.name, version: version, state: state,
                              possibles: possibles, parents: wrap(request.parent)]
          activated = Dict.put(activated, request.name, new_active)

          do_resolve(activated, pending ++ new_pending, overriden)
      end
    end
  end

  defp backtrack(nil, _activated, _overriden) do
    nil
  end

  defp backtrack(Active[state: state] = active, activated, overriden) do
    case active.possibles do
      [] ->
        Enum.find_value(active.parents, fn parent ->
          backtrack(activated[parent], activated, overriden)
        end)

      [version|possibles] ->
        active = active.possibles(possibles).version(version)
        pending = get_deps(active.name, version, overriden)

        activated = Dict.put(state.activated, active.name, active)
        do_resolve(activated, state.pending ++ pending, overriden)
    end
  end

  def get_deps(package, version, overriden) do
    { _, deps, _, _ } = Registry.get_release(package, version)

    Enum.flat_map(deps, fn { name, req } ->
      if name in overriden do
        []
      else
        [Request[name: name, req: req, parent: package]]
      end
    end)
  end

  defp vsn_match?(_version, nil),
    do: true
  defp vsn_match?(version, req) do
    if Regex.regex?(req) do
      version =~ req
    else
      Version.match?(version, req)
    end
  end

  defp wrap(nil), do: []
  defp wrap(arg), do: [arg]
end
