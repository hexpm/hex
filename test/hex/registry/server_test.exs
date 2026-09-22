defmodule Hex.Registry.ServerTest do
  use HexTest.Case
  alias Hex.Registry.Server, as: Registry

  setup do
    Hex.State.put(:offline, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    :ok
  end

  test "dependencies/3 turns an unsatisfiable requirement into an empty constraint" do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(
        tid,
        {{:deps, "hexpm", "parent", "1.0.0"},
         [{"hexpm", "child", "child", "~> 1.0 and >= 2.0.0", false}]}
      )

      %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
    end)

    assert {:ok, [dependency]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    assert dependency.constraint == %Hex.Solver.Constraints.Empty{}

    assert_received {:mix_shell, :error,
                     [
                       "\e[33mPackage parent 1.0.0 can't be used because its requirement " <>
                         "\"~> 1.0 and >= 2.0.0\" for child can never be satisfied: " <>
                         "\"~> 1.0\" and \">= 2.0.0\" are disjoint" <> _
                     ]}

    assert {:ok, [_dependency]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    refute_received {:mix_shell, :error, _}
  end

  describe "failed package fetches" do
    setup do
      Registry.close()
      bypass = Bypass.open()
      repos = Hex.State.fetch!(:repos)
      Hex.State.put(:repos, put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}"))
      Hex.State.put(:offline, false)
      Hex.State.put(:shell_process, self())
      Registry.open(check_version: false, registry_path: tmp_path("fetch_error_cache.ets"))
      {:ok, bypass: bypass}
    end

    test "without a cached copy don't claim to use the cache", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/packages/uncached_package", fn conn ->
        Plug.Conn.resp(conn, 404, "")
      end)

      Registry.prefetch([{"hexpm", "uncached_package"}])
      assert :error = Registry.versions("hexpm", "uncached_package")

      assert_received {:mix_shell, :error, [message]}
      assert message == "Failed to fetch record for uncached_package from registry"
    end

    test "with a cached copy fall back to it", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/packages/cached_package", fn conn ->
        Plug.Conn.resp(conn, 404, "")
      end)

      :sys.replace_state(Registry, fn %{ets: tid} = state ->
        :ets.insert(tid, {{:versions, "hexpm", "cached_package"}, ["1.0.0"]})
        state
      end)

      Registry.prefetch([{"hexpm", "cached_package"}])
      assert {:ok, [_version]} = Registry.versions("hexpm", "cached_package")

      assert_received {:mix_shell, :error, [message]}

      assert message ==
               "Failed to fetch record for cached_package from registry (using cache instead)"
    end
  end

  test "prefetch raises a helpful error in offline mode when a package is not cached" do
    assert_raise Mix.Error,
                 ~r"Hex is running in offline mode and the registry entry for package missing_package is not cached locally",
                 fn ->
                   Registry.prefetch([{"hexpm", "missing_package"}])
                 end
  end
end
