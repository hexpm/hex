defmodule Hex.Resolver do
  alias Hex.Registry
  import Hex.Mix
  require Record

  Record.defrecordp :info, [:deps, :top_level, :backtrack]
  Record.defrecordp :request, [:app, :name, :req, :parent]
  Record.defrecordp :active, [:app, :name, :version, :parents]
  Record.defrecordp :parent, [:name, :version, :requirement]

  def resolve(requests, deps, top_level, locked) do
    {:ok, backtrack} = Agent.start_link(fn -> [] end)

    try do
      info = info(deps: deps, top_level: top_level, backtrack: backtrack)

      optional =
        Enum.into(locked, %{}, fn {name, app, version} ->
          {:ok, req} = Hex.Version.parse_requirement(version)
          parent     = parent(name: "mix.lock", requirement: req)
          request    = request(app: app, name: name, req: req, parent: parent)
          {name, [request]}
        end)

      pending =
        Enum.map(requests, fn {name, app, req, from} ->
          req    = compile_requirement(req, name)
          parent = parent(name: from, requirement: req)
          request(name: name, app: app, req: req, parent: parent)
        end)
        |> Enum.uniq

      if activated = do_resolve(pending, optional, info, %{}) do
        {:ok, activated}
      else
        {:error, error_message(backtrack)}
      end
    after
      Agent.stop(backtrack)
    end
  end

  defp do_resolve([], _optional, _info, activated) do
    Enum.map(activated, fn {name, active(app: app, version: version)} ->
      {name, app, version}
    end) |> Enum.reverse
  end

  defp do_resolve([request(name: name, req: req, parent: parent) = request|pending], optional, info, activated) do
    case activated[name] do
      active(version: version, parents: parents) = active ->
        parents = [parent|parents]
        active  = active(active, parents: parents)

        if version_match?(version, req) do
          activated = Map.put(activated, name, active)
          do_resolve(pending, optional, info, activated)
        else
          add_backtrack_info(name, version, parents, info)
          nil
        end

      nil ->
        {opts, optional} = Map.pop(optional, name)
        opts             = opts || []
        requests         = [request|opts]
        parents          = Enum.map(requests, &request(&1, :parent))

        case get_versions(name, requests) do
          {:ok, versions} ->
            activate(request, pending, versions, optional, info, activated, parents)

          {:error, _requests} ->
            add_backtrack_info(name, nil, parents, info)
            nil
        end
    end
  end

  defp activate(request(app: app, name: name), pending, versions,
                optional, info, activated, parents) do
    Enum.find_value(versions, fn version ->
      {new_pending, new_optional, new_deps} = get_deps(app, name, version, info, activated)
      new_pending = pending ++ new_pending
      new_optional = merge_optional(optional, new_optional)

      new_active = active(app: app, name: name, version: version, parents: parents)
      activated = Map.put(activated, name, new_active)

      info = info(info, deps: new_deps)

      do_resolve(new_pending, new_optional, info, activated)
    end)
  end

  defp get_versions(package, requests) do
    if versions = Registry.get_versions(package) do
      try do
        {versions, _requests} =
          Enum.reduce(requests, {versions, []}, fn request, {versions, requests} ->
            req = request(request, :req)
            case Enum.filter(versions, &version_match?(&1, req)) do
              [] ->
                throw [request | requests]
              versions ->
                {versions, [request | requests]}
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
      overridden_map = overridden_parents(top_level, all_deps, String.to_atom(app))

      {reqs, opts} =
        Enum.reduce(deps, {[], []}, fn {name, app, req, optional}, {reqs, opts} ->
          req = compile_requirement(req, name)
          parent = parent(name: package, version: version, requirement: req)
          request = request(app: app, name: name, req: req, parent: parent)

          cond do
            overridden_map[String.to_atom(app)] ->
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

  # Add a potentially new dependency and its children.
  # This function is used to add Hex packages to the dependency tree which
  # we use in overridden_parents to check overridden status.
  defp attach_dep_and_children(deps, app, children) do
    app = String.to_atom(app)
    dep = Enum.find(deps, &(&1.app == app))

    children =
      Enum.map(children, fn {name, app, _req, _optional} ->
        app = String.to_atom(app)
        name = String.to_atom(name)
        %Mix.Dep{app: app, opts: [hex: name]}
      end)

    new_dep = %{dep | deps: children}

    put_dep(deps, new_dep) ++ children
  end

  # Replace a dependency in the tree
  defp put_dep(deps, new_dep) do
    app = new_dep.app
    Enum.map(deps, fn dep ->
      if dep.app == app, do: new_dep, else: dep
    end)
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
    case Hex.Version.parse_requirement(req) do
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
    Agent.get(agent_pid, &(&1))
    |> merge_backtracks([])
    |> sort_backtracks
    |> Enum.map_join("\n\n", &backtrack_message/1)
    |> Kernel.<>("\n")
  end

  defp add_backtrack_info(name, version, parents, info(backtrack: agent)) do
    info = {name, version, parents}
    Agent.cast(agent, &[info|&1])
  end

  defp merge_backtracks([], acc) do
    acc
  end

  defp merge_backtracks([{name, version, parents}|rest], acc) do
    similar_versions =
      Enum.reduce(rest, [version], fn entry, acc ->
        case entry do
          {^name, version2, ^parents} ->
            [version2 | acc]
          _ ->
            acc
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq

    rest =
      Enum.reject(rest, fn {name2, version, _} ->
        name == name2 and (version in similar_versions or is_nil(version))
      end)

    acc = [{name, similar_versions, parents}|acc]

    merge_backtracks(rest, acc)
  end

  defp sort_backtracks(backtracks) do
    backtracks
    |> Enum.map(fn {name, versions, parents} ->
         parents  = Enum.sort(parents, &sort_parents/2)
         versions = Enum.sort(versions, &(Hex.Version.compare(&1, &2) != :lt))
         {name, versions, parents}
       end)
    |> Enum.sort()
  end

  # TODO: Handle sorting of mix.exs from umbrellas
  defp sort_parents(parent(name: "mix.exs"), _),  do: true
  defp sort_parents(_, parent(name: "mix.exs")),  do: false
  defp sort_parents(parent(name: "mix.lock"), _), do: true
  defp sort_parents(_, parent(name: "mix.lock")), do: false
  defp sort_parents(parent1, parent2),            do: parent1 <= parent2

  defp backtrack_message({name, versions, parents}) do
    versions = if versions != [] do
      " " <> Enum.join(versions, ", ")
    end
    "Conflict on #{name}#{versions}" <>
    "\n  " <> Enum.map_join(parents, "\n  ", &parent_message/1)
  end

  defp parent_message(parent(name: path, version: nil, requirement: req)),
    do: "From #{path}: #{requirement(req)}"
  defp parent_message(parent(name: parent, version: version, requirement: req)),
    do: "From #{parent} #{version}: #{requirement(req)}"

  defp requirement(nil), do: ">= 0.0.0"
  defp requirement(req), do: req.source
end
