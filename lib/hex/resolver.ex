defmodule Hex.Resolver do
  defrecord Info, [:overridden]
  defrecord State, [:activated, :pending]
  defrecord Request, [:name, :req, :parent]
  defrecord Active, [:name, :version, :state, :parents, :possibles]

  alias Hex.Registry

  def resolve(requests, overridden, locked) do
    info = Info[overridden: Enum.into(overridden, HashSet.new)]

    # Make sure to add children of locked dependencies, they may have been
    # overridden before and not been included in the lock
    { activated, pending } =
      Enum.reduce(locked, { HashDict.new, [] }, fn { name, version }, { dict, pending } ->
        active = Active[name: name, version: version, parents: [], possibles: []]
        dict = Dict.put(dict, name, active)
        pending = pending ++ get_deps(name, version, info)
        { dict, pending }
      end)

    req_requests =
      Enum.map(requests, fn { name, req } ->
        Request[name: name, req: compile_requirement(req, name)]
      end)

    pending = pending ++ req_requests
    do_resolve(activated, pending, info)
  end

  defp do_resolve(activated, [], _info) do
    Enum.map(activated, fn { name, Active[version: version] } ->
      { name, version }
    end) |> Enum.reverse
  end

  defp do_resolve(activated, [Request[] = request|pending], info) do
    active = activated[request.name]

    if active do
      possibles = Enum.filter(active.possibles, &vsn_match?(&1, request.req))
      active = active.possibles(possibles).parents(wrap(request.parent) ++ active.parents)

      if vsn_match?(active.version, request.req) do
        activated = Dict.put(activated, request.name, active)
        do_resolve(activated, pending, info)
      else
        backtrack(active, activated, info)
      end
    else
      case get_versions(request.name, request.req) do
        [] ->
          backtrack(activated[request.parent], activated, info)

        [version|possibles] ->
          new_pending = get_deps(request.name, version, info)

          state = State[activated: activated, pending: pending]
          new_active = Active[name: request.name, version: version, state: state,
                              possibles: possibles, parents: wrap(request.parent)]
          activated = Dict.put(activated, request.name, new_active)

          do_resolve(activated, pending ++ new_pending, info)
      end
    end
  end

  defp backtrack(nil, _activated, _info) do
    nil
  end

  defp backtrack(Active[state: state] = active, activated, info) do
    case active.possibles do
      [] ->
        Enum.find_value(active.parents, fn parent ->
          backtrack(activated[parent], activated, info)
        end)

      [version|possibles] ->
        active = active.possibles(possibles).version(version)
        pending = get_deps(active.name, version, info)

        activated = Dict.put(state.activated, active.name, active)
        do_resolve(activated, state.pending ++ pending, info)
    end
  end

  def get_versions(package, req) do
    if versions = Registry.get_versions(package) do
      versions
      |> Enum.filter(&vsn_match?(&1, req))
      |> Enum.reverse
    else
      raise Mix.Error, message: "Unable to find package #{package} in registry"
    end
  end

  def get_deps(package, version, Info[overridden: overridden]) do
    if deps = Registry.get_deps(package, version) do
      Enum.flat_map(deps, fn { name, req } ->
        if Set.member?(overridden, name) do
          []
        else
          [Request[name: name, req: compile_requirement(req, name), parent: package]]
        end
      end)
    else
      raise Mix.Error, message: "Unable to find package version #{package} v#{version} in registry"
    end
  end

  defp compile_requirement(nil, _package) do
    nil
  end

  defp compile_requirement(req, package) when is_binary(req) do
    case Version.parse_requirement(req) do
      { :ok, req } ->
        req
      :error ->
        raise Mix.Error, message: "Invalid requirement #{inspect req} defined for package #{package}"
    end
  end

  defp compile_requirement(req, package) do
    raise Mix.Error, message: "Invalid requirement #{inspect req} defined for package #{package}"
  end

  defp vsn_match?(version, nil), do: true
  defp vsn_match?(version, req), do: Version.match?(version, req)

  defp wrap(nil), do: []
  defp wrap(arg), do: [arg]
end
