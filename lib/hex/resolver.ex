defmodule Hex.Resolver do
  @moduledoc false

  import Hex.Mix
  require Record
  alias Hex.Resolver.Backtracks

  @ets :hex_ets_states

  Record.defrecordp(:info, [:registry, :deps, :top_level, :ets, :repos, :backtracks])
  Record.defrecordp(:request, [:app, :name, :req, :repo, :parent])
  Record.defrecordp(:active, [:app, :repo, :name, :version, :versions, :parents, :state])
  Record.defrecordp(:parent, [:name, :repo, :version, :requirement, :repo_requirement])
  Record.defrecordp(:state, [:activated, :requests, :optional, :deps])

  def resolve(registry, requests, deps, top_level, repos, locked) do
    tid = :ets.new(@ets, [])
    backtracks = Backtracks.start()

    info =
      info(
        registry: registry,
        deps: deps,
        top_level: top_level,
        ets: tid,
        repos: repos,
        backtracks: backtracks
      )

    optional = build_optional(locked)
    pending = build_pending(requests)

    try do
      if solutions = run(pending, optional, info, %{}) do
        solutions = filter_and_sort_solutions(solutions)
        print_verbose_solutions(solutions)

        if activated = select_solution(solutions, top_level) do
          print_verbose_errors(backtracks, registry)
          {:ok, activated}
        else
          {:error, {:version, error_message(backtracks, registry)}}
        end
      else
        {:error, {:version, error_message(backtracks, registry)}}
      end
    catch
      {:repo_conflict, message} ->
        {:error, {:repo, message}}
    after
      Backtracks.stop(backtracks)
      :ets.delete(tid)
    end
  end

  defp build_optional(locked) do
    Enum.into(locked, %{}, fn {repo, name, app, version} ->
      parent = parent(name: "mix.lock", requirement: version, repo_requirement: repo)
      request = request(app: app, name: name, req: version, repo: repo, parent: parent)
      {name, [request]}
    end)
  end

  defp build_pending(requests) do
    Enum.map(requests, fn {repo, name, app, req, from} ->
      parent = parent(name: from, requirement: req, repo_requirement: repo)
      request(name: name, app: app, req: req, repo: repo, parent: parent)
    end)
    |> Enum.uniq()
  end

  defp run([], _optional, _info, activated) do
    solution =
      Enum.map(activated, fn {name, active(repo: repo, app: app, version: version)} ->
        {repo, name, app, version}
      end)
      |> Enum.reverse()

    {:ok, solution}
  end

  defp run(
         [request(repo: repo, name: name, req: req, parent: parent) = request | pending],
         optional,
         info,
         activated
       ) do
    case Map.fetch(activated, name) do
      {:ok, active(repo: active_repo, version: version, parents: parents) = active} ->
        repo = info(info, :repos)[name] || repo
        parents = [parent | parents]
        active = active(active, parents: parents)

        cond do
          active_repo != repo ->
            repo_conflict(name, parents)

          not version_match?(version, req) ->
            Backtracks.add(info(info, :backtracks), repo, name, version, parents)
            backtrack(name, info, activated) || backtrack_parents([parent], info, activated)

          true ->
            activated = Map.put(activated, name, active)
            run(pending, optional, info, activated)
        end

      :error ->
        {opts, optional} = Map.pop(optional, name, [])
        requests = [request | opts]
        parents = Enum.map(requests, &request(&1, :parent))

        case get_versions(repo, name, requests, info) do
          [] ->
            Backtracks.add(info(info, :backtracks), repo, name, nil, parents)
            backtrack_parents(parents, info, activated)

          versions ->
            activate(request, pending, versions, optional, info, activated, parents)
        end
    end
  end

  defp repo_conflict(name, parents) do
    message =
      Hex.Shell.format([
        [:underline, "Failed to use \"", name, "\" because", :reset, "\n"],
        parent_messages(parents)
      ])
      |> IO.iodata_to_binary()

    throw({:repo_conflict, message})
  end

  defp parent_messages(parents) do
    parents =
      parents
      |> Enum.uniq()
      |> Enum.sort()

    Enum.map(parents, fn parent(name: name, repo_requirement: repo) ->
      ["  ", :bright, name, :reset, " requires repo ", :red, repo, :reset, "\n"]
    end)
  end

  defp activate(
         request(app: app, name: name, repo: repo),
         pending,
         [version | versions],
         optional,
         info,
         activated,
         parents
       ) do
    {new_pending, new_optional, new_deps} = get_deps(app, repo, name, version, info, activated)
    new_pending = pending ++ new_pending
    new_optional = merge_optional(optional, new_optional)

    state =
      state(activated: activated, requests: pending, optional: optional, deps: info(info, :deps))

    unless seen_state?(name, version, state, info) do
      repo = info(info, :repos)[name] || repo

      new_active =
        active(
          app: app,
          repo: repo,
          name: name,
          version: version,
          versions: versions,
          parents: parents,
          state: state
        )

      activated = Map.put(activated, name, new_active)
      info = info(info, deps: new_deps)

      run(new_pending, new_optional, info, activated)
    end
  end

  defp activate(_request, _pending, [], _optional, info, activated, parents) do
    backtrack_parents(parents, info, activated)
  end

  defp backtrack(name, info, activated) do
    case activated[name] do
      active(state: state, app: app, repo: repo, name: name, versions: versions, parents: parents) ->
        state(activated: activated, requests: requests, optional: optional, deps: deps) = state
        info = info(info, deps: deps)
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
    not :ets.insert_new(ets, [{{name, version, state}}])
  end

  defp get_versions(repo, name, requests, info(registry: registry)) do
    if versions = registry.versions(repo, name) do
      Enum.reduce(requests, versions, fn request, versions ->
        req = request(request, :req)
        Enum.filter(versions, &version_match?(&1, req))
      end)
      |> Enum.reverse()
    else
      Mix.raise("Unable to find package #{name} in registry")
    end
  end

  defp get_deps(
         app,
         repo,
         package,
         version,
         info(registry: registry, top_level: top_level, deps: all_deps),
         activated
       ) do
    if deps = registry.deps(repo, package, version) do
      packages = Enum.map(deps, &{elem(&1, 0), elem(&1, 1)})
      registry.prefetch(packages)
      all_deps = attach_dep_and_children(all_deps, app, deps)
      overridden_map = overridden_parents(top_level, all_deps, app)

      {reqs, opts} =
        Enum.reduce(deps, {[], []}, fn {dep_repo, name, app, req, optional}, {reqs, opts} ->
          parent =
            parent(
              name: package,
              version: version,
              requirement: req,
              repo: repo,
              repo_requirement: dep_repo
            )

          request = request(app: app, name: name, req: req, parent: parent, repo: dep_repo)

          cond do
            overridden_map[app] ->
              {reqs, opts}

            optional && !activated[name] ->
              {reqs, [request | opts]}

            true ->
              {[request | reqs], opts}
          end
        end)

      {Enum.reverse(reqs), Enum.reverse(opts), all_deps}
    else
      Mix.raise("Unable to find package version #{package} #{version} in registry")
    end
  end

  defp merge_optional(optional, new_optional) do
    new_optional =
      Enum.into(new_optional, %{}, fn request(name: name) = request ->
        {name, [request]}
      end)

    Map.merge(optional, new_optional, fn _, v1, v2 -> v1 ++ v2 end)
  end

  # Due to backtracking we may have multiple possible solutions with no clear indication of
  # which is preferable. We try to find a solution with the highest versions of dependencies
  # by looking at:
  #
  #  1. The top level dependencies
  #  2. The rest of the dependencies
  #  3. If that fails, use the solution with the least dependencies
  #  4. Finally pick the first solution we found
  defp select_solution(solutions, top_level) do
    case solutions do
      [best | solutions] -> select_solution(solutions, top_level, best)
      [] -> nil
    end
  end

  defp select_solution([], _top_level, best) do
    best
  end

  defp select_solution([best | solutions], top_level, best) do
    select_solution(solutions, top_level, best)
  end

  defp select_solution([solution | solutions], top_level, best) do
    select_solution_top_level(solution, best, solutions, top_level)
  end

  defp select_solution_top_level(left, right, rest, top_level) do
    compare_left = Enum.filter(left, fn {_, _, app, _} -> app in top_level end)
    compare_right = Enum.filter(right, fn {_, _, app, _} -> app in top_level end)

    case compare_solutions(compare_left, compare_right, 0) do
      :left ->
        select_solution(rest, top_level, left)

      :right ->
        select_solution(rest, top_level, right)

      :equal ->
        select_solution_all(left, right, rest, top_level)
    end
  end

  defp select_solution_all(left, right, rest, top_level) do
    left_apps = Enum.map(left, fn {_, _, app, _} -> app end)
    right_apps = Enum.map(right, fn {_, _, app, _} -> app end)
    compare_apps = shared_elements(left_apps, right_apps)
    compare_left = Enum.filter(left, fn {_, _, app, _} -> app in compare_apps end)
    compare_right = Enum.filter(right, fn {_, _, app, _} -> app in compare_apps end)

    case compare_solutions(compare_left, compare_right, 0) do
      :left ->
        select_solution(rest, top_level, left)

      :right ->
        select_solution(rest, top_level, right)

      :equal ->
        select_solution_length(left, right, rest, top_level)
    end
  end

  defp select_solution_length(left, right, rest, top_level) do
    if length(right) > length(left) do
      select_solution(rest, top_level, left)
    else
      select_solution(rest, top_level, right)
    end
  end

  defp shared_elements(left, right) do
    all = left ++ right
    uniq = Enum.uniq(all)
    all -- uniq
  end

  defp compare_solutions(
         [{_, _, app, left_version} | lefts],
         [{_, _, app, right_version} | rights],
         count
       ) do
    case Hex.Version.compare(left_version, right_version) do
      :lt -> compare_solutions(lefts, rights, count - 1)
      :gt -> compare_solutions(lefts, rights, count + 1)
      :eq -> compare_solutions(lefts, rights, count)
    end
  end

  defp compare_solutions([], [], count) do
    cond do
      count > 0 -> :left
      count < 0 -> :right
      count == 0 -> :equal
    end
  end

  defp error_message(backtracks, registry) do
    backtracks
    |> Backtracks.collect()
    |> Enum.map(&Backtracks.message(&1, registry))
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n\n")
    |> pre_message()
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

  defp filter_and_sort_solutions(solutions) do
    solutions
    |> List.wrap()
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.map(fn {:ok, solution} -> solution end)
    |> Enum.uniq()
    |> Enum.map(&Enum.sort/1)
  end

  defp print_verbose_solutions(solutions) do
    if Hex.State.fetch!(:resolve_verbose) do
      Hex.Shell.info("\nSolutions:")

      Enum.each(solutions, fn solution ->
        Hex.Shell.info("")

        Enum.each(solution, fn {_repo, name, _app, version} ->
          Hex.Shell.info("  #{name} #{version}")
        end)
      end)

      Hex.Shell.info("")
    end
  end

  defp print_verbose_errors(backtracks, registry) do
    if Hex.State.fetch!(:resolve_verbose) do
      message = error_message(backtracks, registry)
      if message != "", do: Hex.Shell.info("\n\n" <> message)
    end
  end
end
