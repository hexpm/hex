defmodule Hex.ResolverTest do
  use HexTest.Case
  import HexTest.ResolverHelper
  alias Hex.Registry.Server, as: Registry

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

  defp expand_versions(registry) do
    Enum.flat_map(registry, fn {repo, package, versions, deps} ->
      Enum.map(versions, fn version ->
        {repo, package, version, deps}
      end)
    end)
  end

  defp setup_registry(path, registry) do
    registry = expand_versions(registry)
    HexTest.Case.create_test_registry(path, registry)
    Registry.close()
    Registry.open(registry_path: path)
    :ok
  end

  setup do
    Hex.State.put(:offline, true)
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

    deps = [ex_plex: "< 0.1.0", decimal: nil]
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

    deps = [eric: nil, jose: nil]
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

  test "issue #571" do
    # earmark and ex_doc at 2018-06-09
    setup_registry(Path.join(test_tmp(), "cache.ets"), [
      {:hexpm, :earmark,
       ~w(0.1.0 0.1.1 0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7 0.1.8 0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.2.0 0.2.1 1.0.0 1.0.1 1.0.2 1.0.3 1.1.0 1.1.1 1.2.0 1.2.1 1.2.2 1.2.3 1.2.4 1.2.5),
       []},
      {:hexpm, :ex_doc, ~w(0.5.1 0.5.2 0.6.0 0.6.1 0.6.2 0.7.0 0.7.1 0.7.2), []},
      {:hexpm, :ex_doc,
       ~w(0.7.3 0.8.0 0.8.1 0.8.2 0.8.3 0.8.4 0.9.0 0.10.0 0.11.0 0.11.1 0.11.2 0.11.3 0.11.4 0.11.5),
       [{:earmark, "~> 0.1.17 or ~> 0.2", true}]},
      {:hexpm, :ex_doc, ~w(0.12.0), [earmark: "~> 0.2"]},
      {:hexpm, :ex_doc, ~w(0.13.0 0.13.1 0.13.2 0.14.0 0.14.1 0.14.2 0.14.3 0.14.4 0.14.5),
       [earmark: "~> 1.0"]},
      {:hexpm, :ex_doc,
       ~w(0.15.0 0.15.1 0.16.0 0.16.1 0.16.2 0.16.3 0.16.4 0.17.0 0.17.1 0.17.2 0.18.0 0.18.1 0.18.2 0.18.3),
       [earmark: "~> 1.1"]}
    ])

    deps = [earmark: "~> 0.1", ex_doc: "~> 0.11"]
    locked = []
    assert equal?(locked(earmark: "0.2.1", ex_doc: "0.12.0"), resolve(deps, locked))

    deps = [earmark: "~> 0.1", ex_doc: "~> 0.11"]
    locked = [earmark: "0.2.1"]
    assert equal?(locked(earmark: "0.2.1", ex_doc: "0.12.0"), resolve(deps, locked))

    deps = [earmark: "~> 0.1", ex_doc: "~> 0.11"]
    locked = [ex_doc: "0.12.0"]
    assert equal?(locked(earmark: "0.2.1", ex_doc: "0.12.0"), resolve(deps, locked))
  end
end
