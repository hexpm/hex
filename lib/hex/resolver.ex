defmodule Hex.Resolver do
  import Hex.Mix
  require Record
  alias Hex.Registry
  alias Hex.Resolver.Backtracks

  Record.defrecordp :info, [:deps, :top_level, :ets]
  Record.defrecordp :request, [:app, :name, :req, :parent]
  Record.defrecordp :active, [:app, :name, :version, :versions, :parents, :state]
  Record.defrecordp :parent, [:name, :version, :requirement]
  Record.defrecordp :state, [:activated, :requests, :optional, :deps]

  def resolve(requests, deps, top_level, locked) do
    tid = :ets.new(:hex_ets_states, [])
    Backtracks.start

    info     = info(deps: deps, top_level: top_level, ets: tid)
    optional = build_optional(locked)
    pending  = build_pending(requests)

    try do
      if activated = run(pending, optional, info, %{}),
        do: {:ok, activated},
      else: {:error, error_message()}
    catch
      :duplicate_state ->
        {:error, error_message()}
    after
      Backtracks.stop
      :ets.delete(tid)
    end
  end

  defp build_optional(locked) do
    Enum.into(locked, %{}, fn {name, app, version} ->
      parent     = parent(name: "mix.lock", requirement: version)
      request    = request(app: app, name: name, req: version, parent: parent)
      {name, [request]}
    end)
  end

  defp build_pending(requests) do
    Enum.map(requests, fn {name, app, req, from} ->
      parent = parent(name: from, requirement: req)
      request(name: name, app: app, req: req, parent: parent)
    end)
    |> Enum.uniq
  end

  defp run([], _optional, _info, activated) do
    Enum.map(activated, fn {name, active(app: app, version: version)} ->
      {name, app, version}
    end) |> Enum.reverse
  end

  defp run([request(name: name, req: req, parent: parent) = request|pending], optional, info, activated) do
    case activated[name] do
      active(version: version, parents: parents) = active ->
        parents = [parent|parents]
        active  = active(active, parents: parents)

        if version_match?(version, req) do
          activated = Map.put(activated, name, active)
          run(pending, optional, info, activated)
        else
          add_backtrack_info(name, version, parents)
          backtrack_parents([parent], info, activated) ||
            backtrack(name, info, activated)
        end

      nil ->
        {opts, optional} = Map.pop(optional, name, [])
        requests         = [request|opts]
        parents          = Enum.map(requests, &request(&1, :parent))

        case get_versions(name, requests) do
          [] ->
            add_backtrack_info(name, nil, parents)
            backtrack_parents(parents, info, activated)
          versions ->
            activate(request, pending, versions, optional, info, activated, parents)
        end
    end
  end

  defp activate(request(app: app, name: name), pending, [version|versions],
                optional, info, activated, parents) do
    {new_pending, new_optional, new_deps} = get_deps(app, name, version, info, activated)
    new_pending = pending ++ new_pending
    new_optional = merge_optional(optional, new_optional)

    state = state(activated: activated, requests: pending, optional: optional, deps: info(info, :deps))

    if seen_state?(name, version, state, info) do
      new_active = active(app: app, name: name, version: version, versions: versions, parents: parents, state: state)
      activated = Map.put(activated, name, new_active)

      info = info(info, deps: new_deps)

      run(new_pending, new_optional, info, activated)
    else
      throw :duplicate_state
    end
  end

  defp activate(_request, _pending, [], _optional, info, activated, parents) do
    backtrack_parents(parents, info, activated)
  end

  defp backtrack(name, info, activated) do
    case activated[name] do
      active(state: state, app: app, name: name, versions: versions, parents: parents) ->
        state(activated: activated, requests: requests, optional: optional, deps: deps) = state

        info    = info(info, deps: deps)
        request = request(app: app, name: name)

        activate(request, requests, versions, optional, info, activated, parents)
      nil ->
        nil
    end
  end

  defp backtrack_parents(parents, info, activated) do
    Enum.find_value(parents, fn parent(name: name) ->
      backtrack(name, info, activated)
    end)
  end

  defp seen_state?(name, version, state(activated: activated) = state, info(ets: ets)) do
    activated = Enum.into(activated, %{}, fn {k, v} -> {k, active(v, state: nil)} end)
    state = state(state, activated: activated, deps: nil)
    :ets.insert_new(ets, [{{name, version, state}}])
  end

  defp get_versions(package, requests) do
    if versions = Registry.versions(package) do
      Enum.reduce(requests, versions, fn request, versions ->
        req = request(request, :req)
        Enum.filter(versions, &version_match?(&1, req))
      end)
      |> Enum.reverse
    else
      Mix.raise "Unable to find package #{package} in registry"
    end
  end

  defp get_deps(app, package, version, info(top_level: top_level, deps: all_deps), activated) do
    if deps = Registry.deps(package, version) do
      dep_names = Enum.map(deps, &elem(&1, 0))
      Registry.prefetch(dep_names)
      all_deps = attach_dep_and_children(all_deps, app, deps)
      overridden_map = overridden_parents(top_level, all_deps, app)

      {reqs, opts} =
        Enum.reduce(deps, {[], []}, fn {name, app, req, optional}, {reqs, opts} ->
          parent  = parent(name: package, version: version, requirement: req)
          request = request(app: app, name: name, req: req, parent: parent)

          cond do
            overridden_map[app] ->
              {reqs, opts}
            optional && !activated[name] ->
              {reqs, [request|opts]}
            true ->
              {[request|reqs], opts}
          end
        end)

      {Enum.reverse(reqs), Enum.reverse(opts), all_deps}
    else
      Mix.raise "Unable to find package version #{package} #{version} in registry"
    end
  end

  defp merge_optional(optional, new_optional) do
    new_optional =
      Enum.into(new_optional, %{}, fn request(name: name) = request ->
        {name, [request]}
      end)
    Map.merge(optional, new_optional, fn _, v1, v2 -> v1 ++ v2 end)
  end

  defp add_backtrack_info(name, version, parents) do
    Backtracks.add(name, version, parents)
  end

  defp error_message do
    Backtracks.collect
    |> Enum.map(&Backtracks.message/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n\n")
    |> pre_message
  end

  defp pre_message(message) do
    # ugly hack to check if we should print the pre-release explanation
    if message =~ "*\n" do
      message <>
        "\n* This requirement does not match pre-releases. " <>
        "To match pre-releases include a pre-release in the " <>
        "requirement, such as: \"~> 2.0-beta\".\n"
    else
      message
    end
  end
end
