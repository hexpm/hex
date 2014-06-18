defmodule Hex.Resolver do
  alias Hex.Registry

  import Hex.Mix, only: [version_match?: 2]

  require Record
  Record.defrecord :info, [:overridden]
  Record.defrecord :state, [:activated, :pending, :optional]
  Record.defrecord :request, [:name, :req, :parent]
  Record.defrecord :active, [:name, :version, :state, :parents, :possibles]

  def resolve(requests, overridden, locked) do
    info = info(overridden: Enum.into(overridden, HashSet.new))

    { activated, pending, optional } =
      Enum.reduce(locked, { HashDict.new, [], HashDict.new }, &locked(&1, &2, info))

    req_requests =
      Enum.map(requests, fn { name, req } ->
        request(name: name, req: compile_requirement(req, name))
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

    active = active(name: name, version: version, parents: [], possibles: [])
    activated = Dict.put(activated, name, active)

    {activated, pending, optional}
  end

  defp do_resolve(activated, [], _optional, _info) do
    Enum.map(activated, fn { name, active(version: version) } ->
      { name, version }
    end) |> Enum.reverse
  end

  defp do_resolve(activated, [request(name: name, req: req, parent: parent) = request|pending], optional, info) do
    case activated[name] do
      active(version: version, possibles: possibles, parents: parents) = active ->
        possibles = Enum.filter(possibles, &version_match?(&1, req))
        active = active(active, possibles: possibles, parents: wrap(parent) ++ parents)

        if version_match?(version, req) do
          activated = Dict.put(activated, name, active)
          do_resolve(activated, pending, optional, info)
        else
          backtrack(active, activated, info)
        end

      nil ->
        { opts, optional } = Dict.pop(optional, name)
        requests = [request] ++ (opts || [])

        case get_versions(name, requests) do
          { :error, request(parent: parent) } ->
            backtrack(activated[parent], activated, info)

          { :ok, [version|possibles] } ->
            {new_pending, new_optional} = get_deps(name, version, info)
            new_pending = pending ++ new_pending
            new_optional = merge_optional(optional, new_optional)

            state = state(activated: activated, pending: pending, optional: optional)
            new_active = active(name: name, version: version, state: state,
                                possibles: possibles, parents: wrap(parent))
            activated = Dict.put(activated, name, new_active)

            do_resolve(activated, new_pending, new_optional, info)
        end
    end
  end

  defp backtrack(nil, _activated, _info) do
    nil
  end

  defp backtrack(active(name: name, possibles: possibles, parents: parents, state: state) = active, activated, info) do
    case possibles do
      [] ->
        Enum.find_value(parents, fn parent ->
          backtrack(activated[parent], activated, info)
        end)

      [version|possibles] ->
        state(activated: activated, pending: pending, optional: optional) = state

        active = active(active, possibles: possibles, version: version)
        {new_pending, new_optional} = get_deps(name, version, info)
        pending = pending ++ new_pending
        optional = merge_optional(optional, new_optional)

        activated = Dict.put(activated, name, active)
        do_resolve(activated, pending, optional, info)
    end
  end

  def get_versions(package, requests) do
    if versions = Registry.get_versions(package) do
      try do
        versions =
          Enum.reduce(requests, versions, fn request(req: req) = request, versions ->
            versions = Enum.filter(versions, &version_match?(&1, req))
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
      Mix.raise "Unable to find package #{package} in registry"
    end
  end

  def get_deps(package, version, info(overridden: overridden)) do
    if deps = Registry.get_deps(package, version) do
      {reqs, opts} =
        Enum.reduce(deps, { [], [] }, fn { name, req, optional }, { reqs, opts } ->
          request = request(name: name, req: compile_requirement(req, name), parent: package)

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
      Mix.raise "Unable to find package version #{package} v#{version} in registry"
    end
  end

  defp merge_optional(optional, new_optional) do
    new_optional =
      Enum.map(new_optional, fn request(name: name) = request ->
        {name, [request]}
      end)
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
        Mix.raise "Invalid requirement #{inspect req} defined for package #{package}"
    end
  end

  defp compile_requirement(req, package) do
    Mix.raise "Invalid requirement #{inspect req} defined for package #{package}"
  end

  defp wrap(nil), do: []
  defp wrap(arg), do: [arg]
end
