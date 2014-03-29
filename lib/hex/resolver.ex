defmodule Hex.Resolver do
  defrecord Info, [:overriden, :locked]
  defrecord State, [:activated, :pending]
  defrecord Request, [:name, :req, :parent]
  defrecord Active, [:name, :version, :state, :parents, :possibles]

  alias Hex.Registry

  def resolve(requests, overriden, locked) do
    info = Info[overriden: HashSet.new(overriden), locked: HashSet.new(locked)]

    { activated, pending } =
      Enum.reduce(locked, { HashDict.new, [] }, fn { name, version }, { dict, pending } ->
        verify_existence(name, version)
        active = Active[name: name, version: version, parents: [], possibles: []]
        dict = Dict.put(dict, name, active)
        pending = pending ++ get_deps(name, version, info)
        { dict, pending }
      end)

    req_requests =
      Enum.map(requests, fn { name, req } ->
        verify_existence(name)
        Request[name: name, req: req]
      end)

    try do
      pending = pending ++ req_requests
      unlocked_pending(pending, info)
      do_resolve(activated, pending, info)
    catch
      :throw, :restart ->
        resolve(requests, overriden, locked)
    end
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
      versions = Registry.get_versions(request.name)
        |> Enum.filter(&vsn_match?(&1, request.req))
        |> Enum.reverse

      case versions do
        [] ->
          backtrack(activated[request.parent], activated, info)

        [version|possibles] ->
          new_pending = get_deps(request.name, version, info)
          unlocked_pending(new_pending, info)

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
        unlocked_pending(pending, info)

        activated = Dict.put(state.activated, active.name, active)
        do_resolve(activated, state.pending ++ pending, info)
    end
  end

  def get_deps(package, version, Info[overriden: overriden]) do
    { _, deps } = Registry.get_release(package, version)

    pending =
      Enum.flat_map(deps, fn { name, req } ->
        if Set.member?(overriden, name) do
          []
        else
          verify_existence(name)
          [Request[name: name, req: req, parent: package]]
        end
      end)

    pending
  end

  defp unlocked_pending(pending, Info[locked: locked]) do
    Enum.find(pending, fn Request[name: name] ->
      unless Set.member?(locked, name) do
        if Hex.RemoteConverger.update_registry("Updating registry...") == { :ok, :new } do
          throw :restart
        end
        :ok
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

  defp verify_existence(name) do
    unless Registry.exists?(name) do
      result = Hex.RemoteConverger.update_registry("Found unknown package #{name}, updating registry...")
      if match?({ :ok, _ }, result) do
        unless Registry.exists?(name) do
          Mix.shell.info("Package still not found, is your lockfile bad?")
          raise Mix.Error
        end
      else
        raise Mix.Error
      end
    end
  end

  defp verify_existence(name, version) do
    unless Registry.exists?(name, version) do
      result = Hex.RemoteConverger.update_registry("Found unknown package #{name} #{version}, updating registry...")
      if match?({ :ok, _ }, result) do
        unless Registry.exists?(name, version) do
          Mix.shell.info("Package still not found, is your lockfile bad?")
          raise Mix.Error
        end
      else
        raise Mix.Error
      end
    end
  end
end
