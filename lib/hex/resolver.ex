defmodule Hex.Resolver do
  alias Hex.Registry
  import Hex.Mix
  require Record

  Record.defrecordp :info, [:deps, :top_level, :backtrack_agent]
  Record.defrecordp :state, [:activated, :pending, :optional]
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
        HashDict.put(acc, name, request)
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

  defp do_resolve([request(app: app, name: name, req: req, parent: parent) = request|pending], optional, info, activated) do
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
        opts = List.wrap(opts || [])
        requests = [request|opts]

        case get_versions(name, requests) do
          {:error, request(parent: parent)} ->
            backtrack_message(name, nil, [parent|opts], info)
            backtrack(activated[parent], info, activated)

          {:ok, [version|possibles]} ->
            {new_pending, new_optional} = get_deps(name, version, info)
            new_pending = pending ++ new_pending
            new_optional = merge_optional(optional, new_optional)

            state = state(activated: activated, pending: pending, optional: optional)
            new_active = active(app: app, name: name, version: version, state: state,
                                possibles: possibles, parents: [parent])
            activated = HashDict.put(activated, name, new_active)

            do_resolve(new_pending, new_optional, info, activated)
        end
    end
  end

  defp backtrack(nil, _info, _activated) do
    nil
  end

  defp backtrack(active(name: name, possibles: possibles, parents: parents, state: state) = active, info, activated) do
    case possibles do
      [] ->
        Enum.find_value(parents, fn
          {{parent, _version}, _req} when not parent in ~w(mix_exs mix_lock)a ->
            backtrack(activated[parent], info, activated)
          _ ->
            nil
        end)

      [version|possibles] ->
        state(activated: activated, pending: pending, optional: optional) = state

        active = active(active, possibles: possibles, version: version)
        {new_pending, new_optional} = get_deps(name, version, info)
        pending = pending ++ new_pending
        optional = merge_optional(optional, new_optional)

        activated = HashDict.put(activated, name, active)
        do_resolve(pending, optional, info, activated)
    end
  end

  defp get_versions(package, requests) do
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

        {:ok, Enum.reverse(versions)}
      catch
        :throw, request ->
          {:error, request}
      end

    else
      Mix.raise "Unable to find package #{package} in registry"
    end
  end

  defp get_deps(package, version, info(top_level: top_level, deps: all_deps)) do
    if deps = Registry.get_deps(package, version) do
      upper_breadths = down_to(top_level, all_deps, String.to_atom(package))

      {reqs, opts} =
        Enum.reduce(deps, {[], []}, fn {name, app, req, optional}, {reqs, opts} ->
          req = compile_requirement(req, name)
          parent = {{package, version}, req}
          request = request(app: app, name: name, req: req, parent: parent)

          cond do
            was_overridden?(upper_breadths, String.to_atom(app)) ->
              {reqs, opts}
            optional ->
              {reqs, [request|opts]}
            true ->
              {[request|reqs], opts}
          end
        end)

        {Enum.reverse(reqs), Enum.reverse(opts)}
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
