defmodule Explex do
  defrecord State, [:activated, :pending]
  defrecord Request, [:name, :req, :parent]
  defrecord Active, [:name, :version, :state, :parents, :possibles]

  defexception Error, [:message]

  @ets_table :explex_ets_registry
  @dets_table :explex_dets_registry

  def start(opts // []) do
    filename = opts[:registry_path] || Path.join(Mix.Utils.mix_home, "explex.dets")
    ram_file = opts[:ram_file] || false

    dets_opts = [
      access: :read,
      file: filename,
      ram_file: ram_file,
      type: :duplicate_bag ]

    ets_opts = [
      :set,
      :named_table,
      :public ]

    case :dets.open_file(@dets_table, dets_opts) do
      { :ok, @dets_table } -> :ok
      { :error, reason } -> raise Explex.Error, message: "failed to open registry file, reason: #{inspect reason}"
    end
    :ets.new(@ets_table, ets_opts)

    # TODO: Check registry version
    :ok
  end

  def stop do
    :ets.delete(@ets_table)
    :dets.close(@dets_table)
    :ok
  end

  def resolve(requests, locked // []) do
    # TODO: Check if locked deps are valid (they should exist in registry)

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
    case :ets.lookup(@ets_table, package) do
      [] ->
        load_package(package)
        :ets.lookup_element(@ets_table, package, 2)
      [{ ^package, versions }] ->
        versions
    end
  end

  defp get_deps(package, version) do
    deps =
      case :ets.lookup(@ets_table, { package, version }) do
        [] ->
          load_package(package)
          :ets.lookup_element(@ets_table, { package, version }, 2)
        [{ { ^package, ^version }, deps }] ->
          deps
      end

    Enum.map(deps, fn { name, req } ->
      Request[name: name, req: req, parent: package]
    end)
  end

  defp load_package(package) do
    packages = :dets.lookup(@dets_table, package)
    versions =
      Enum.map(packages, fn { name, version, deps } ->
        :ets.insert(@ets_table, { { name, version }, deps })
        version
      end)

    versions = Enum.sort(versions, &Version.gt?/2)
    :ets.insert(@ets_table, { package, versions })
  end

  defp wrap(nil), do: []
  defp wrap(arg), do: [arg]
end
