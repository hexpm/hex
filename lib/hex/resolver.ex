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
      Enum.reduce(locked, HashDict.new, fn {name, app, version}, acc ->
        {:ok, req} = Version.parse_requirement(version)
        request = request(app: app, name: name, req: req, parent: {"mix.lock", req})
        HashDict.put(acc, name, [request])
      end)

    pending =
      Enum.map(requests, fn {name, app, req, from} ->
        req = compile_requirement(req, name)
        request(name: name, app: app, req: req, parent: {from, req})
      end)
      |> Enum.uniq

    if activated = do_resolve(pending, optional, info, HashDict.new) do
      Agent.stop(agent_pid)
      {:ok, activated}
    else
      messages = Agent.get(agent_pid, & &1)
      Agent.stop(agent_pid)
      {:error, Enum.join(messages, "\n\n")}
    end
  end

  defp do_resolve([], _optional, _info, activated) do
    Enum.map(activated, fn {name, active(app: app, version: version)} ->
      {name, app, version}
    end) |> Enum.reverse
  end

  defp do_resolve([request(name: name, req: req, parent: parent) = request|pending], optional, info, activated) do
    case activated[name] do
      active(version: version, possibles: possibles, parents: parents) = active ->
        possibles = Enum.filter(possibles, &version_match?(&1, req))
        active = active(active, possibles: possibles, parents: [parent|parents])

        if version_match?(version, req) do
          activated = HashDict.put(activated, name, active)
          do_resolve(pending, optional, info, activated)
        else
          backtrack_message(name, version, [parent|parents], info)
          backtrack(active, info, activated)
        end

      nil ->
        {opts, optional} = HashDict.pop(optional, name)
        opts = opts || []
        requests = [request|opts]
        parents = Enum.map(requests, &request(&1, :parent))

        case get_versions(name, requests) do
          {:error, _requests} ->
            backtrack_message(name, nil, parents, info)
            backtrack(activated[parent], info, activated)

          {:ok, versions} ->
            activate([request|pending], versions, optional, info, activated, parents)
        end
    end
  end

  defp backtrack(nil, _info, _activated) do
    nil
  end

  defp backtrack(active(app: app, name: name, possibles: possibles, parents: parents, state: state) = active, info, activated) do
    case possibles do
      [] ->
        Enum.find_value(parents, fn
          {{parent, _version}, _req} when not parent in ~w(mix_exs mix_lock)a ->
            backtrack(activated[parent], info, activated)
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

        activated = HashDict.put(activated, name, active)
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
    activated = HashDict.put(activated, name, new_active)

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
          parent = {{package, version}, req}
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
      Enum.into(new_optional, HashDict.new, fn request(name: name) = request ->
        {name, [request]}
      end)
    HashDict.merge(optional, new_optional, fn _, v1, v2 -> v1 ++ v2 end)
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

  defp backtrack_message(name, version, parents, info(backtrack_agent: agent)) do
    string = [
      "Looking up alternatives for conflicting requirements on #{name}",
      if(version, do: "  Activated version: #{version}"),
      "  " <> Enum.map_join(parents, "\n  ", &parent/1)]
      |> Enum.filter(& &1)
      |> Enum.join("\n")

    Agent.cast(agent, &[string|&1])
  end

  defp parent({path, req}) when is_binary(path),
    do: "From #{path}: #{requirement(req)}"
  defp parent({{parent, version}, req}),
    do: "From #{parent} v#{version}: #{requirement(req)}"

  defp requirement(nil), do: ""
  defp requirement(req), do: req.source
end
