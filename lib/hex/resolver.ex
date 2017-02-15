defmodule Hex.Resolver do
  import Hex.Mix
  require Record
  alias Hex.Resolver.Backtracks

  @ets :hex_ets_states

  Record.defrecordp :info, [:registry, :deps, :top_level, :ets, :repos]
  Record.defrecordp :request, [:app, :name, :req, :repo, :parent]
  Record.defrecordp :active, [:app, :repo, :name, :version, :versions, :parents, :state]
  Record.defrecordp :parent, [:name, :repo, :version, :requirement, :repo_requirement]
  Record.defrecordp :state, [:activated, :requests, :optional, :deps]

  def resolve(registry, requests, deps, top_level, repos, locked) do
    tid = :ets.new(@ets, [])
    Backtracks.start

    info     = info(registry: registry, deps: deps, top_level: top_level, ets: tid, repos: repos)
    optional = build_optional(locked)
    pending  = build_pending(requests)

    try do
      if activated = run(pending, optional, info, %{}),
        do: {:ok, activated},
      else: {:error, {:version, error_message()}}
    catch
      {:repo_conflict, message} ->
        {:error, {:repo, message}}
      :duplicate_state ->
        {:error, {:version, error_message()}}
    after
      Backtracks.stop
      :ets.delete(tid)
    end
  end

  defp build_optional(locked) do
    Enum.into(locked, %{}, fn {repo, name, app, version} ->
      parent     = parent(name: "mix.lock", requirement: version, repo_requirement: repo)
      request    = request(app: app, name: name, req: version, repo: repo, parent: parent)
      {name, [request]}
    end)
  end

  defp build_pending(requests) do
    Enum.map(requests, fn {repo, name, app, req, from} ->
      parent = parent(name: from, requirement: req, repo_requirement: repo)
      request(name: name, app: app, req: req, repo: repo, parent: parent)
    end)
    |> Enum.uniq
  end

  defp run([], _optional, _info, activated) do
    Enum.map(activated, fn {name, active(repo: repo, app: app, version: version)} ->
      {repo, name, app, version}
    end) |> Enum.reverse
  end

  defp run([request(repo: repo, name: name, req: req, parent: parent) = request|pending], optional, info, activated) do
    case activated[name] do
      active(repo: active_repo, version: version, parents: parents) = active ->
        repo = info(info, :repos)[name] || repo
        parents = [parent|parents]
        active = active(active, parents: parents)

        cond do
          active_repo != repo ->
            repo_conflict(name, parents)
          not version_match?(version, req) ->
            add_backtrack_info(repo, name, version, parents)
            backtrack_parents([parent], info, activated) ||
              backtrack(name, info, activated)
          true ->
            activated = Map.put(activated, name, active)
            run(pending, optional, info, activated)
        end

      nil ->
        {opts, optional} = Map.pop(optional, name, [])
        requests         = [request|opts]
        parents          = Enum.map(requests, &request(&1, :parent))

        case get_versions(repo, name, requests, info) do
          [] ->
            add_backtrack_info(repo, name, nil, parents)
            backtrack_parents(parents, info, activated)
          versions ->
            activate(request, pending, versions, optional, info, activated, parents)
        end
    end
  end

  defp repo_conflict(name, parents) do
    message = IO.ANSI.format([
        :underline, "Failed to use \"", name, "\" because", :reset, "\n",
        parent_messages(parents)
      ])
      |> IO.iodata_to_binary
    throw {:repo_conflict, message}
  end

  defp parent_messages(parents) do
    parents =
      parents
      |> Enum.uniq
      |> Enum.sort

    Enum.map(parents, fn parent(name: name, repo_requirement: repo) ->
      ["  ", :bright, name, :reset, " requires repo ", :red, repo, :reset, "\n"]
    end)
  end

  defp activate(request(app: app, name: name, repo: repo), pending,
                [version|versions], optional, info, activated, parents) do
    {new_pending, new_optional, new_deps} = get_deps(app, repo, name, version, info, activated)
    new_pending = pending ++ new_pending
    new_optional = merge_optional(optional, new_optional)

    state = state(activated: activated, requests: pending, optional: optional, deps: info(info, :deps))

    if seen_state?(name, version, state, info) do
      repo = info(info, :repos)[name] || repo
      new_active = active(app: app, repo: repo, name: name, version: version, versions: versions, parents: parents, state: state)
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
      active(state: state, app: app, repo: repo, name: name, versions: versions, parents: parents) ->
        state(activated: activated, requests: requests, optional: optional, deps: deps) = state

        info    = info(info, deps: deps)
        request = request(repo: repo, app: app, name: name)

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

  defp get_versions(repo, package, requests, info(registry: registry)) do
    if versions = registry.versions(repo, package) do
      Enum.reduce(requests, versions, fn request, versions ->
        req = request(request, :req)
        Enum.filter(versions, &version_match?(&1, req))
      end)
      |> Enum.reverse
    else
      Mix.raise "Unable to find package #{package} in registry"
    end
  end

  defp get_deps(app, repo, package, version,
                info(registry: registry, top_level: top_level, deps: all_deps),
                activated) do
    if deps = registry.deps(repo, package, version) do
      packages = Enum.map(deps, &{elem(&1, 0), elem(&1, 1)})
      registry.prefetch(packages)
      all_deps = attach_dep_and_children(all_deps, app, deps)
      overridden_map = overridden_parents(top_level, all_deps, app)

      {reqs, opts} =
        Enum.reduce(deps, {[], []}, fn {dep_repo, name, app, req, optional}, {reqs, opts} ->
          parent  = parent(name: package, version: version, requirement: req, repo: repo, repo_requirement: dep_repo)
          request = request(app: app, name: name, req: req, parent: parent, repo: dep_repo)

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

  defp add_backtrack_info(repo, name, version, parents) do
    Backtracks.add(repo, name, version, parents)
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
