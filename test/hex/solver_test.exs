defmodule Hex.SolverTest do
  use HexTest.Case
  import HexTest.SolverHelper
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

  setup do
    Hex.State.put(:offline, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    :ok
  end

  test "simple" do
    assert solve(foo: nil, bar: nil) == %{foo: "0.2.1", bar: "0.2.0"}
    assert solve(foo: "0.2.1", bar: "0.2.0") == %{foo: "0.2.1", bar: "0.2.0"}
    assert solve(foo: "0.2.0", bar: "0.2.0") == %{foo: "0.2.0", bar: "0.2.0"}

    assert solve(bar: nil, foo: "~> 0.3.0") == """
           Because "your app" depends on "foo ~> 0.3.0" which doesn't match any versions, version solving failed.\
           """

    assert solve(foo: "~> 0.3.0", bar: nil) == """
           Because "your app" depends on "foo ~> 0.3.0" which doesn't match any versions, version solving failed.\
           """

    assert solve(bar: "~> 0.3.0", foo: nil) == """
           Because "your app" depends on "bar ~> 0.3.0" which doesn't match any versions, version solving failed.\
           """

    assert solve(foo: nil, bar: "~> 0.3.0") == """
           Because "your app" depends on "bar ~> 0.3.0" which doesn't match any versions, version solving failed.\
           """
  end

  test "backtrack" do
    assert solve(decimal: "0.2.0", ex_plex: "0.2.0") == %{decimal: "0.2.0", ex_plex: "0.2.0"}
    assert solve(decimal: "0.1.0", ex_plex: ">= 0.1.0") == %{decimal: "0.1.0", ex_plex: "0.1.2"}
    assert solve(decimal: nil, ex_plex: "< 0.1.0") == %{decimal: "0.2.1", ex_plex: "0.0.1"}
    assert solve(ex_plex: "< 0.1.0", decimal: nil) == %{decimal: "0.2.1", ex_plex: "0.0.1"}
    assert solve(decimal: "0.1.0", ex_plex: "< 0.1.0") == %{decimal: "0.1.0", ex_plex: "0.0.1"}

    assert solve(ex_plex: "~> 0.0.2", decimal: "0.1.0") == """
           Because "ex_plex >= 0.0.2 and < 0.1.0" depends on "decimal 0.1.1" and "your app" depends on "decimal 0.1.0", "ex_plex >= 0.0.2 and < 0.1.0" is forbidden.
           So, because "your app" depends on "ex_plex ~> 0.0.2", version solving failed.\
           """

    assert solve(decimal: "0.1.0", ex_plex: "~> 0.0.2") == """
           Because "ex_plex >= 0.0.2 and < 0.1.0" depends on "decimal 0.1.1" and "your app" depends on "decimal 0.1.0", "ex_plex >= 0.0.2 and < 0.1.0" is forbidden.
           So, because "your app" depends on "ex_plex ~> 0.0.2", version solving failed.\
           """

    assert solve(ex_plex: "0.0.2", decimal: nil) == """
           Because "ex_plex >= 0.0.2 and < 0.1.0" depends on "decimal 0.1.1" which doesn't match any versions, "ex_plex >= 0.0.2 and < 0.1.0" is forbidden.
           So, because "your app" depends on "ex_plex 0.0.2", version solving failed.\
           """

    assert solve(decimal: nil, ex_plex: "0.0.2") == """
           Because "ex_plex >= 0.0.2 and < 0.1.0" depends on "decimal 0.1.1" which doesn't match any versions, "ex_plex >= 0.0.2 and < 0.1.0" is forbidden.
           So, because "your app" depends on "ex_plex 0.0.2", version solving failed.\
           """
  end

  test "complete backtrack" do
    assert solve(jose: nil, eric: nil) == %{jose: "0.2.1", eric: "0.0.2"}
    assert solve(eric: nil, jose: nil) == %{jose: "0.2.1", eric: "0.0.2"}
  end

  test "backtrack with multiple parents" do
    assert solve(phoenix: "~> 1.1.3", phoenix_ecto: "~> 2.0", phoenix_live_reload: "~> 1.0") == %{
             ecto: "1.1.0",
             phoenix: "1.1.3",
             phoenix_ecto: "2.0.1",
             phoenix_live_reload: "1.0.3",
             poison: "1.5.2"
           }

    assert solve(phoenix: nil, phoenix_ecto: "~> 2.0", phoenix_live_reload: "~> 1.0") == %{
             ecto: "1.1.0",
             phoenix: "1.1.3",
             phoenix_ecto: "2.0.1",
             phoenix_live_reload: "1.0.3",
             poison: "1.5.2"
           }
  end

  test "locked" do
    assert solve([decimal: nil, ex_plex: nil], decimal: "0.2.0") == %{
             decimal: "0.2.0",
             ex_plex: "0.2.0"
           }

    assert solve([decimal: nil, ex_plex: nil], decimal: "0.1.0") == %{
             decimal: "0.1.0",
             ex_plex: "0.1.2"
           }

    assert solve([decimal: nil, ex_plex: nil], decimal: "0.0.1") == %{
             decimal: "0.0.1",
             ex_plex: "0.0.1"
           }

    assert solve([decimal: "0.1.0", ex_plex: nil], ex_plex: "0.1.0") == %{
             decimal: "0.1.0",
             ex_plex: "0.1.0"
           }

    assert solve([decimal: "0.1.0", ex_plex: nil], ex_plex: "0.1.0", decimal: "0.1.0") == %{
             decimal: "0.1.0",
             ex_plex: "0.1.0"
           }

    assert solve([decimal: nil, ex_plex: nil], ex_plex: "0.1.0", decimal: "0.1.0") == %{
             decimal: "0.1.0",
             ex_plex: "0.1.0"
           }

    assert solve([], ex_plex: "0.1.0", decimal: "0.1.0") == %{}
  end

  test "failure due to locked dep" do
    assert solve([ex_plex: "0.1.0", decimal: nil], decimal: "0.0.1") == """
           Because "lock" specifies "decimal 0.0.1" and "ex_plex >= 0.1.0 and < 0.2.0" depends on "decimal ~> 0.1.0", "lock" is incompatible with "ex_plex >= 0.1.0 and < 0.2.0".
           And because "your app" depends on "lock", "ex_plex >= 0.1.0 and < 0.2.0" is forbidden.
           So, because "your app" depends on "ex_plex 0.1.0", version solving failed.\
           """

    assert solve([decimal: nil, ex_plex: "0.1.0"], decimal: "0.0.1") == """
           Because "lock" specifies "decimal 0.0.1" and "ex_plex >= 0.1.0 and < 0.2.0" depends on "decimal ~> 0.1.0", "lock" is incompatible with "ex_plex >= 0.1.0 and < 0.2.0".
           And because "your app" depends on "lock", "ex_plex >= 0.1.0 and < 0.2.0" is forbidden.
           So, because "your app" depends on "ex_plex 0.1.0", version solving failed.\
           """

    assert solve([ex_plex: "0.1.0", decimal: "~> 0.0.1"], decimal: "0.0.1") == """
           Because "ex_plex >= 0.1.0 and < 0.2.0" depends on "decimal ~> 0.1.0" and "your app" depends on "decimal ~> 0.0.1", "ex_plex >= 0.1.0 and < 0.2.0" is forbidden.
           So, because "your app" depends on "ex_plex 0.1.0", version solving failed.\
           """

    assert solve([decimal: "~> 0.0.1", ex_plex: "0.1.0"], decimal: "0.0.1") == """
           Because "ex_plex >= 0.1.0 and < 0.2.0" depends on "decimal ~> 0.1.0" and "your app" depends on "decimal ~> 0.0.1", "ex_plex >= 0.1.0 and < 0.2.0" is forbidden.
           So, because "your app" depends on "ex_plex 0.1.0", version solving failed.\
           """
  end

  test "pre-release" do
    assert solve(beta: "~> 1.0 and > 1.0.0") == %{beta: "1.0.0"}
    assert solve(beta: "~> 1.0-beta and > 1.0.0-beta") == %{beta: "1.0.0"}
    assert solve(beta: "~> 1.1-beta and > 1.1.0-beta") == %{beta: "1.1.0-beta"}
  end

  test "only mix.exs conflicts" do
    assert solve(decimal: "~> 0.0.1", ex_plex: "0.2.0") == """
           Because "ex_plex >= 0.2.0" depends on "decimal ~> 0.2.0" and "your app" depends on "decimal ~> 0.0.1", "ex_plex >= 0.2.0" is forbidden.
           So, because "your app" depends on "ex_plex 0.2.0", version solving failed.\
           """
  end

  test "optional" do
    assert solve(ex_doc: nil, has_optional: nil) == %{ex_doc: "0.0.2", has_optional: "0.1.0"}
  end

  test "multiple repos" do
    add_repo("repo2")

    assert solve("repo2/repo2_deps": ">= 0.0.0") == %{
             "repo2/repo2_deps": "0.1.0",
             "repo2/poison": "2.0.0"
           }

    assert solve("repo2/hexpm_deps": ">= 0.0.0") == %{
             poison: "2.0.0",
             "repo2/hexpm_deps": "0.1.0"
           }

    assert assert solve("repo2/repo2_deps": ">= 0.0.0", "hexpm/phoenix": ">= 0.0.0") == """
                  Failed to use "poison" because
                    phoenix requires repo hexpm
                    repo2_deps requires repo repo2\n
                  """
  end

  test "implicit override repo" do
    add_repo("repo2")

    assert solve("repo2/repo2_deps": ">= 0.0.0", poison: ">= 0.0.0") == %{
             poison: "2.0.0",
             "repo2/repo2_deps": "0.1.0"
           }
  end
end
