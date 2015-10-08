defmodule Hex.Resolver do
  alias Hex.Registry
  import Hex.Mix
  require Record

  Record.defrecordp :info, [:deps, :top_level, :backtrack_agent]
  Record.defrecordp :state, [:activated, :pending, :optional, :deps]
  Record.defrecordp :request, [:app, :name, :req, :parent]
  Record.defrecordp :active, [:app, :name, :version, :state, :parents, :possibles]

  def resolve(requests, deps, locked) do
    {:ok, agent_pid} = Agent.start_link(fn -> [] end)

    top_level = top_level(deps)
    info = info(deps: deps, top_level: top_level, backtrack_agent: agent_pid)

    optional =
      Enum.into(locked, %{}, fn {name, app, version} ->
        {:ok, req} = Version.parse_requirement(version)
        parent     = parent("mix.lock", nil, req, nil)
        request    = request(app: app, name: name, req: req, parent: parent)
        {name, [request]}
      end)

    pending =
      Enum.map(requests, fn {name, app, req, from} ->
        req    = compile_requirement(req, name)
        parent = parent(from, nil, req, nil)
        request(name: name, app: app, req: req, parent: parent)
      end)
      |> Enum.uniq

    if activated = do_resolve(pending, optional, info, %{}) do
      Agent.stop(agent_pid)
      {:ok, activated}
    else
      {:error, error_message(agent_pid)}
    end
  end

  defp do_resolve([], _optional, _info, activated) do
    Enum.map(activated, fn {name, active(app: app, version: version)} ->
      {name, app, version}
    end) |> Enum.reverse
  end

  defp do_resolve([request(name: name, req: req, parent: parent) = request|pending], optional, info, activated) do
    case activated[name] do
      active(version: version, possibles: possibles, parents: parents, state: state) = active ->
        possibles = Enum.filter(possibles, &version_match?(&1, req))
        parents   = [parent|parents]
        active    = active(active, possibles: possibles, parents: parents)

        if version_match?(version, req) do
          activated = Map.put(activated, name, active)
          do_resolve(pending, optional, info, activated)
        else
          add_backtrack_info(name, version, parents, info)
          backtrack(active, state, info, activated)
        end

      nil ->
        {opts, optional} = Map.pop(optional, name)
        opts = opts || []
        requests = [request|opts]
        parents = Enum.map(requests, &request(&1, :parent))

        case get_versions(name, requests) do
          {:ok, versions} ->
            activate([request|pending], versions, optional, info, activated, parents)

          {:error, _requests} ->
            add_backtrack_info(name, nil, parents, info)
            case Map.fetch(activated, parent) do
              {:ok, active(state: state) = parent} ->
                backtrack(parent, state, info, activated)
              :error ->
                nil
            end
        end
    end
  end

  defp backtrack(nil, nil, _info, _activated) do
    nil
  end

  defp backtrack(nil, _state, _info, _activated) do
    nil
  end

  defp backtrack(active(state: state) = active, nil, info, activated) do
    backtrack(active, state, info, activated)
  end

  defp backtrack(active(app: app, name: name, possibles: possibles, parents: parents) = active, state, info, activated) do
    case possibles do
      [] ->
        Enum.find_value(parents, fn
          %{name: parent, state: state} when not parent in ~w(mix.exs mix.lock) ->
            backtrack(activated[parent], state, info, activated)
          _ ->
            nil
        end)

      [version|possibles] ->
        state(activated: activated, pending: pending, optional: optional, deps: deps) = state

        active = active(active, possibles: possibles, version: version)
        info = info(info, deps: deps)
        {new_pending, new_optional, new_deps} = get_deps(app, name, version, info, activated)
        pending = pending ++ new_pending
        optional = merge_optional(optional, new_optional)
        info = info(info, deps: new_deps)

        activated = Map.put(activated, name, active)
        do_resolve(pending, optional, info, activated)
    end
  end

  defp activate([request(app: app, name: name)|pending], [version|possibles],
                optional, info(deps: deps) = info, activated, parents) do
    {new_pending, new_optional, new_deps} = get_deps(app, name, version, info, activated)
    new_pending = pending ++ new_pending
    new_optional = merge_optional(optional, new_optional)

    state = state(activated: activated, pending: pending, optional: optional, deps: deps)
    new_active = active(app: app, name: name, version: version, state: state,
                        possibles: possibles, parents: parents)
    activated = Map.put(activated, name, new_active)

    info = info(info, deps: new_deps)

    do_resolve(new_pending, new_optional, info, activated)
  end

  defp get_versions(package, requests) do
    if versions = Registry.get_versions(package) do
      try do
        {versions, _requests} =
          Enum.reduce(requests, {versions, []}, fn request, {versions, requests} ->
            req = request(request, :req)
            versions = Enum.filter(versions, &version_match?(&1, req))
            if versions == [] do
              throw [request|requests]
            else
              {versions, [request|requests]}
            end
          end)

        {:ok, Enum.reverse(versions)}
      catch
        :throw, requests ->
          {:error, requests}
      end

    else
      Mix.raise "Unable to find package #{package} in registry"
    end
  end

  defp get_deps(app, package, version, info(top_level: top_level, deps: all_deps), activated) do
    if deps = Registry.get_deps(package, version) do
      all_deps = attach_dep_and_children(all_deps, app, deps)

      upper_breadths = down_to(top_level, all_deps, String.to_atom(app))

      {reqs, opts} =
        Enum.reduce(deps, {[], []}, fn {name, app, req, optional}, {reqs, opts} ->
          req = compile_requirement(req, name)
          parent = parent(package, version, req, nil)
          request = request(app: app, name: name, req: req, parent: parent)

          cond do
            was_overridden?(upper_breadths, String.to_atom(app)) ->
              {reqs, opts}
            optional && !activated[name] ->
              {reqs, [request|opts]}
            true ->
              {[request|reqs], opts}
          end
        end)

        {Enum.reverse(reqs), Enum.reverse(opts), all_deps}
    else
      Mix.raise "Unable to find package version #{package} v#{version} in registry"
    end
  end

  # Add a potentially new dependency and its children.
  # This function is used to add Hex packages to the dependency tree which
  # we use in down_to to check overridden status.
  defp attach_dep_and_children(deps, app, children) do
    app = String.to_atom(app)
    dep = Enum.find(deps, &(&1.app == app))

    children =
      Enum.map(children, fn {name, app, _req, _optional} ->
        app = String.to_atom(app)
        name = String.to_atom(name)
        %Mix.Dep{app: app, opts: [hex: name]}
      end)

    new_dep = put_in(dep.deps, children)

    put_dep(deps, new_dep) ++ children
  end

  # Replace a dependency in the tree
  defp put_dep(deps, new_dep) do
    Enum.reduce(deps, [], fn dep, deps ->
      if dep.app == new_dep.app do
        [new_dep|deps]
      else
        [dep|deps]
      end
    end)
    |> Enum.reverse
  end

  defp merge_optional(optional, new_optional) do
    new_optional =
      Enum.into(new_optional, %{}, fn request(name: name) = request ->
        {name, [request]}
      end)
    Map.merge(optional, new_optional, fn _, v1, v2 -> v1 ++ v2 end)
  end

  defp compile_requirement(nil, _package) do
    nil
  end

  defp compile_requirement(req, package) when is_binary(req) do
    case Version.parse_requirement(req) do
      {:ok, req} ->
        req
      :error ->
        Mix.raise "Invalid requirement #{inspect req} defined for package #{package}"
    end
  end

  defp compile_requirement(req, package) do
    Mix.raise "Invalid requirement #{inspect req} defined for package #{package}"
  end

  defp error_message(agent_pid) do
    backtrack_info = Agent.get(agent_pid, & &1)
    Agent.stop(agent_pid)
    backtrack_info = remove_useless_backtracks(backtrack_info)
    messages =
      backtrack_info
      |> Enum.map(fn {name, version, parents} -> {name, version, Enum.sort(parents, &sort_parents/2)} end)
      |> Enum.sort()
      |> Enum.map(&backtrack_message/1)
    Enum.join(messages, "\n\n") <> "\n"
  end

  defp add_backtrack_info(name, version, parents, info(backtrack_agent: agent)) do
    info = {name, version, parents}
    Agent.cast(agent, &[info|&1])
  end

  defp remove_useless_backtracks(backtracks) do
    backtracks = Enum.map(backtracks, fn {name, version, parents} ->
      {name, version, new_set(parents)}
    end)

    Enum.reject(backtracks, fn {name1, version1, parents1} ->
      count = Enum.count(backtracks, fn {name2, version2, parents2} ->
        name1 == name2 and version1 == version2 and Set.subset?(parents1, parents2)
      end)
      # We will always match ourselves once
      count > 1
    end)
  end

  defp sort_parents(%{name: "mix.exs"}, _),  do: true
  defp sort_parents(_, %{name: "mix.exs"}),  do: false
  defp sort_parents(%{name: "mix.lock"}, _), do: true
  defp sort_parents(_, %{name: "mix.lock"}), do: false
  defp sort_parents(parent1, parent2),       do: parent1 <= parent2

  defp backtrack_message({name, version, parents}) do
    [ "Looking up alternatives for conflicting requirements on #{name}",
      if(version, do: "  Activated version: #{version}"),
      "  " <> Enum.map_join(parents, "\n  ", &parent/1)]
    |> Enum.filter(& &1)
    |> Enum.join("\n")
  end

  defp parent(%{name: path, version: nil, req: req}),
    do: "From #{path}: #{requirement(req)}"
  defp parent(%{name: parent, version: version, req: req}),
    do: "From #{parent} v#{version}: #{requirement(req)}"

  defp requirement(nil), do: ">= 0.0.0"
  defp requirement(req), do: req.source

  defp parent(parent, version, req, state),
    do: %{name: parent, version: version, req: req, state: state}

  if Version.compare("1.2.0", System.version) == :gt do
    defp new_set(enum), do: Enum.into(enum, HashSet.new)
  else
    defp new_set(enum), do: Enum.into(enum, MapSet.new)
  end
end
