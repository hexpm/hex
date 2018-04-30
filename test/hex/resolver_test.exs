defmodule Hex.ResolverTest do
  use HexTest.Case
  alias Hex.Registry.Server, as: Registry

  defp resolve(reqs, locked \\ [], repos \\ %{}) do
    reqs = Enum.reverse(reqs)
    deps = deps(reqs)
    top_level = Enum.map(deps, &elem(&1, 1))
    reqs = reqs(reqs)
    locked = locked(locked)

    [reqs, locked]
    |> Enum.concat()
    |> Enum.map(&{elem(&1, 0), elem(&1, 1)})
    |> Registry.prefetch()

    case Hex.Resolver.resolve(Registry, reqs, deps, top_level, repos, locked) do
      {:ok, dict} -> dict
      {:error, {_reason, messages}} -> messages <> "\n"
    end
  end

  defp config({app, req}), do: {"hexpm", Atom.to_string(app), req}
  defp config({repo, app, req}), do: {Atom.to_string(repo), Atom.to_string(app), req}

  defp deps(reqs) do
    Enum.into(reqs, %{}, fn dep ->
      {_repo, name, _req} = config(dep)
      {name, {false, %{}}}
    end)
  end

  defp reqs(reqs) do
    Enum.map(reqs, fn dep ->
      {repo, name, req} = config(dep)
      {repo, name, name, req, "mix.exs"}
    end)
  end

  defp locked(locked) do
    Enum.map(locked, fn dep ->
      {repo, name, req} = config(dep)
      {repo, name, name, req}
    end)
  end

  defp equal?(locked, resolved) do
    Enum.sort(locked) == Enum.sort(resolved)
  end

  setup do
    Hex.State.put(:offline?, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    :ok
  end

  test "simple" do
    deps = [foo: nil, bar: nil]
    assert equal?(locked(foo: "0.2.1", bar: "0.2.0"), resolve(deps))

    deps = [foo: "0.2.1", bar: "0.2.0"]
    assert equal?(locked(foo: "0.2.1", bar: "0.2.0"), resolve(deps))

    deps = [foo: "0.2.0", bar: "0.2.0"]
    assert equal?(locked(foo: "0.2.0", bar: "0.2.0"), resolve(deps))

    deps = [bar: nil, foo: "~> 0.3.0"]

    assert resolve(deps) == """
           Failed to use "foo" because there are no packages that matches the requirement
             mix.exs specifies ~> 0.3.0\n
           """

    deps = [foo: "~> 0.3.0", bar: nil]

    assert resolve(deps) == """
           Failed to use \"foo\" because there are no packages that matches the requirement
             mix.exs specifies ~> 0.3.0\n
           """

    deps = [bar: "~> 0.3.0", foo: nil]

    assert resolve(deps) == """
           Failed to use "bar" because there are no packages that matches the requirement
             mix.exs specifies ~> 0.3.0\n
           """

    deps = [foo: nil, bar: "~> 0.3.0"]

    assert resolve(deps) == """
           Failed to use "bar" because there are no packages that matches the requirement
             mix.exs specifies ~> 0.3.0\n
           """
  end

  test "backtrack" do
    deps = [decimal: "0.2.0", ex_plex: "0.2.0"]
    assert equal?(locked(decimal: "0.2.0", ex_plex: "0.2.0"), resolve(deps))

    deps = [decimal: "0.1.0", ex_plex: ">= 0.1.0"]
    assert equal?(locked(decimal: "0.1.0", ex_plex: "0.1.2"), resolve(deps))

    deps = [decimal: nil, ex_plex: "< 0.1.0"]
    assert equal?(locked(decimal: "0.2.1", ex_plex: "0.0.1"), resolve(deps))

    deps = [decimal: "0.1.0", ex_plex: "< 0.1.0"]
    assert equal?(locked(decimal: "0.1.0", ex_plex: "0.0.1"), resolve(deps))

    deps = [ex_plex: "~> 0.0.2", decimal: "0.1.0"]

    assert resolve(deps) == """
           Failed to use "decimal" (version 0.1.0) because
             ex_plex (version 0.0.2) requires 0.1.1
             mix.exs specifies 0.1.0\n
           """

    deps = [decimal: "0.1.0", ex_plex: "~> 0.0.2"]

    assert resolve(deps) == """
           Failed to use "decimal" (version 0.1.0) because
             ex_plex (version 0.0.2) requires 0.1.1
             mix.exs specifies 0.1.0\n
           """

    deps = [ex_plex: "0.0.2", decimal: nil]

    assert resolve(deps) == """
           Failed to use "decimal" (versions 0.0.1 to 0.2.1) because
             ex_plex (version 0.0.2) requires 0.1.1
             mix.exs specifies >= 0.0.0\n
           """

    deps = [decimal: nil, ex_plex: "0.0.2"]

    assert resolve(deps) == """
           Failed to use "decimal" (versions 0.0.1 to 0.2.1) because
             ex_plex (version 0.0.2) requires 0.1.1
             mix.exs specifies >= 0.0.0\n
           """
  end

  test "complete backtrack" do
    deps = [jose: nil, eric: nil]
    assert equal?(locked(jose: "0.2.1", eric: "0.0.2"), resolve(deps))
  end

  test "backtrack with multiple parents" do
    deps = [phoenix: "~> 1.1.3", phoenix_ecto: "~> 2.0", phoenix_live_reload: "~> 1.0"]

    assert equal?(
             locked(
               ecto: "1.1.0",
               phoenix: "1.1.3",
               phoenix_ecto: "2.0.1",
               phoenix_live_reload: "1.0.3",
               poison: "1.5.2"
             ),
             resolve(deps)
           )

    deps = [phoenix: nil, phoenix_ecto: "~> 2.0", phoenix_live_reload: "~> 1.0"]

    assert equal?(
             locked(
               ecto: "1.1.0",
               phoenix: "1.1.3",
               phoenix_ecto: "2.0.1",
               phoenix_live_reload: "1.0.3",
               poison: "1.5.2"
             ),
             resolve(deps)
           )
  end

  test "locked" do
    locked = [decimal: "0.2.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert equal?(locked(decimal: "0.2.0", ex_plex: "0.2.0"), resolve(deps, locked))

    locked = [decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert equal?(locked(decimal: "0.1.0", ex_plex: "0.1.2"), resolve(deps, locked))

    locked = [decimal: "0.0.1"]
    deps = [decimal: nil, ex_plex: nil]
    assert equal?(locked(decimal: "0.0.1", ex_plex: "0.0.1"), resolve(deps, locked))

    locked = [ex_plex: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert equal?(locked(decimal: "0.1.0", ex_plex: "0.1.0"), resolve(deps, locked))

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert equal?(locked(decimal: "0.1.0", ex_plex: "0.1.0"), resolve(deps, locked))

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert equal?(locked(decimal: "0.1.0", ex_plex: "0.1.0"), resolve(deps, locked))

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = []
    assert equal?([], resolve(deps, locked))
  end

  test "failure due to locked dep" do
    locked = [decimal: "0.0.1"]
    deps = [ex_plex: "0.1.0", decimal: nil]

    assert resolve(deps, locked) == """
           Failed to use "decimal" (version 0.0.1) because
             ex_plex (version 0.1.0) requires ~> 0.1.0
             mix.lock specifies 0.0.1\n
           """

    locked = [decimal: "0.0.1"]
    deps = [decimal: nil, ex_plex: "0.1.0"]

    assert resolve(deps, locked) == """
           Failed to use "decimal" (version 0.0.1) because
             ex_plex (version 0.1.0) requires ~> 0.1.0
             mix.lock specifies 0.0.1\n
           """

    locked = [decimal: "0.0.1"]
    deps = [ex_plex: "0.1.0", decimal: "~> 0.0.1"]

    assert resolve(deps, locked) == """
           Failed to use "decimal" (version 0.0.1) because
             ex_plex (version 0.1.0) requires ~> 0.1.0
             mix.lock specifies 0.0.1\n
           """

    locked = [decimal: "0.0.1"]
    deps = [decimal: "~> 0.0.1", ex_plex: "0.1.0"]

    assert resolve(deps, locked) == """
           Failed to use "decimal" (version 0.0.1) because
             ex_plex (version 0.1.0) requires ~> 0.1.0
             mix.lock specifies 0.0.1\n
           """
  end

  test "pre-release message" do
    deps = [beta: "~> 1.0 and > 1.0.0"]

    assert resolve(deps) == """
           Failed to use "beta" because there are no packages that matches the requirement
             mix.exs specifies ~> 1.0 and > 1.0.0 *

           * This requirement does not match pre-releases. To match pre-releases include a pre-release in the requirement, such as: \"~> 2.0-beta\".\n
           """

    deps = [beta: "~> 1.0-beta and > 1.0.0-beta"]
    assert equal?(locked(beta: "1.1.0-beta"), resolve(deps))
  end

  test "only mix.exs conflicts" do
    deps = [decimal: "~> 0.0.1", ex_plex: "0.2.0"]

    assert resolve(deps, []) == """
           Failed to use "decimal" (version 0.0.1) because
             ex_plex (version 0.2.0) requires ~> 0.2.0
             mix.exs specifies ~> 0.0.1\n
           """
  end

  test "optional" do
    deps = [ex_doc: nil, has_optional: nil]
    assert equal?(locked(ex_doc: "0.0.2", has_optional: "0.1.0"), resolve(deps))
  end

  test "multiple repos" do
    add_repo("repo2")

    deps = [{:repo2, :repo2_deps, ">= 0.0.0"}]

    assert equal?(
             locked([{:repo2, :repo2_deps, "0.1.0"}, {:repo2, :poison, "2.0.0"}]),
             resolve(deps, [], %{"repos_deps" => "repo2"})
           )

    deps = [{:repo2, :hexpm_deps, ">= 0.0.0"}]

    assert equal?(
             locked([{:hexpm, :poison, "2.0.0"}, {:repo2, :hexpm_deps, "0.1.0"}]),
             resolve(deps, [], %{"repos_deps" => "repo2"})
           )

    deps = [{:repo2, :repo2_deps, ">= 0.0.0"}, {:hexpm, :phoenix, ">= 0.0.0"}]

    assert assert resolve(deps, [], %{"repos_deps" => "repo2", "phoenix" => "hexpm"}) == """
                  Failed to use \"poison\" because
                    phoenix requires repo hexpm
                    repo2_deps requires repo repo2\n
                  """
  end

  test "implicit override repo" do
    add_repo("repo2")

    deps = [{:repo2, :repo2_deps, ">= 0.0.0"}, {:hexpm, :poison, ">= 0.0.0"}]

    assert equal?(
             locked([{:hexpm, :poison, "2.0.0"}, {:repo2, :repo2_deps, "0.1.0"}]),
             resolve(deps, [], %{"repos_deps" => "repo2", "poison" => "hexpm"})
           )
  end

  defp add_repo(repo) do
    config = %{
      url: repo,
      public_key: nil,
      auth_key: nil,
      api_url: nil,
      api_key: nil
    }

    Hex.State.update!(:repos, &Map.put(&1, repo, config))
  end
end
