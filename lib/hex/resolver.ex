defmodule Hex.Resolver do
  defrecord Info, [:overridden]
  defrecord State, [:activated, :pending, :optional]
  defrecord Request, [:name, :req, :parent]
  defrecord Active, [:name, :version, :state, :parents, :possibles]

  alias Hex.Registry

  def resolve(requests, overridden, locked) do
    info = Info[overridden: Enum.into(overridden, HashSet.new)]

    { activated, pending, optional } =
      Enum.reduce(locked, { HashDict.new, [], HashDict.new }, &locked(&1, &2, info))

    req_requests =
      Enum.map(requests, fn { name, req } ->
        Request[name: name, req: compile_requirement(req, name)]
      end)

    pending = pending ++ req_requests
    do_resolve(activated, pending, optional, info)
  end

  defp locked({name, version}, {activated, pending, optional}, info) do
    # Make sure to add children of locked dependencies, they may have been
    # overridden before and not been included in the lock

    {new_pending, new_optional} = get_deps(name, version, info)
    pending = pending ++ new_pending
    optional = merge_optional(optional, new_optional)

    active = Active[name: name, version: version, parents: [], possibles: []]
    activated = Dict.put(activated, name, active)

    {activated, pending, optional}
  end

  defp do_resolve(activated, [], _optional, _info) do
    Enum.map(activated, fn { name, Active[version: version] } ->
      { name, version }
    end) |> Enum.reverse
  end

  defp do_resolve(activated, [Request[] = request|pending], optional, info) do
    active = activated[request.name]

    if active do
      possibles = Enum.filter(active.possibles, &vsn_match?(&1, request.req))
      active = active.possibles(possibles).parents(wrap(request.parent) ++ active.parents)

      if vsn_match?(active.version, request.req) do
        activated = Dict.put(activated, request.name, active)
        do_resolve(activated, pending, optional, info)
      else
        backtrack(active, activated, info)
      end
    else

      { opts, optional } = Dict.pop(optional, request.name)
      requests = [request] ++ (opts || [])

      case get_versions(request.name, requests) do
        { :error, request } ->
          backtrack(activated[request.parent], activated, info)

        { :ok, [version|possibles] } ->
          {new_pending, new_optional} = get_deps(request.name, version, info)
          new_pending = pending ++ new_pending
          new_optional = merge_optional(optional, new_optional)

          state = State[activated: activated, pending: pending, optional: optional]
          new_active = Active[name: request.name, version: version, state: state,
                              possibles: possibles, parents: wrap(request.parent)]
          activated = Dict.put(activated, request.name, new_active)

          do_resolve(activated, new_pending, new_optional, info)
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
        {pending, optional} = get_deps(active.name, version, info)
        pending = state.pending ++ pending
        optional = merge_optional(state.optional, optional)

        activated = Dict.put(state.activated, active.name, active)
        do_resolve(activated, pending, optional, info)
    end
  end

  def get_versions(package, requests) do
    if versions = Registry.get_versions(package) do
      try do
        versions =
          Enum.reduce(requests, versions, fn Request[] = request, versions ->
            versions = Enum.filter(versions, &vsn_match?(&1, request.req))
            if versions == [] do
              throw request
            else
              versions
            end
          end)

        { :ok, Enum.reverse(versions) }
      catch
        :throw, request ->
          { :error, request }
      end

    else
      raise Mix.Error, message: "Unable to find package #{package} in registry"
    end
  end

  def get_deps(package, version, Info[overridden: overridden]) do
    if deps = Registry.get_deps(package, version) do
      {reqs, opts} = 
        Enum.reduce(deps, { [], [] }, fn { name, req, optional }, { reqs, opts } ->
          request = Request[name: name, req: compile_requirement(req, name), parent: package]

          cond do
            Set.member?(overridden, name) ->
              { reqs, opts }
            optional ->
              { reqs, [request|opts] }
            true ->
              { [request|reqs], opts }
          end
        end)

        { Enum.reverse(reqs), Enum.reverse(opts) }
    else
      raise Mix.Error, message: "Unable to find package version #{package} v#{version} in registry"
    end
  end

  defp merge_optional(optional, new_optional) do
    new_optional = Enum.map(new_optional, &{&1.name, [&1]})
    Dict.merge(optional, new_optional, fn _, v1, v2 -> v1 ++ v2 end)
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

  defp vsn_match?(_version, nil), do: true
  defp vsn_match?(version, req), do: Version.match?(version, req)

  defp wrap(nil), do: []
  defp wrap(arg), do: [arg]
end
