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

  test "dependencies/3 warns about an unsatisfiable requirement in each release" do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      for package <- ["parent", "other_parent"] do
        :ets.insert(
          tid,
          {{:deps, "hexpm", package, "1.0.0"},
           [
             {"hexpm", "child", "child", "~> 1.0", false},
             {"hexpm", "other_child", "other_child", "~> 1.0 and >= 2.0.0", false}
           ]}
        )
      end

      %{
        state
        | fetched:
            MapSet.union(fetched, MapSet.new([{"hexpm", "parent"}, {"hexpm", "other_parent"}]))
      }
    end)

    assert {:ok, [child, other_child]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    assert {:ok, [^child, ^other_child]} = Registry.dependencies("hexpm", "other_parent", "1.0.0")
    assert other_child.constraint == %Hex.Solver.Constraints.Empty{}

    assert_received {:mix_shell, :error, ["\e[33mPackage parent 1.0.0 can't be used" <> _]}
    assert_received {:mix_shell, :error, ["\e[33mPackage other_parent 1.0.0 can't be used" <> _]}
  end

  test "app_dependencies/3 accepts application names" do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(
        tid,
        {{:deps, "hexpm", "parent", "1.0.0"},
         [
           {"hexpm", "child", "child_2", "~> 1.0", false},
           {"hexpm", "b58", "B58", "~> 1.0", false},
           {"hexpm", "lfe_dep", "lfe-Dep", "~> 1.0", false}
         ]}
      )

      %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
    end)

    assert {:ok, [%{label: "child_2"}, %{label: "B58"}, %{label: "lfe-Dep"}]} =
             Registry.app_dependencies("hexpm", "parent", "1.0.0")
  end

  test "app_dependencies/3 rejects application names that aren't directory names" do
    for app <- [
          "../child",
          "child/..",
          "child\\..",
          "..",
          ".",
          ".child",
          "",
          "_child",
          "1child",
          "-child"
        ] do
      :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
        :ets.insert(
          tid,
          {{:deps, "hexpm", "parent", "1.0.0"}, [{"hexpm", "child", app, "~> 1.0", false}]}
        )

        %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
      end)

      message =
        "Package parent 1.0.0 can't be used because its dependency child " <>
          "has the invalid application name #{inspect(app)}"

      assert_raise Mix.Error, message, fn ->
        Registry.app_dependencies("hexpm", "parent", "1.0.0")
      end
    end
  end

  test "dependencies/3 makes a dependency with an invalid application name required and unsatisfiable" do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(tid, [
        {{:deps, "hexpm", "parent", "1.0.0"},
         [
           {"hexpm", "child", "../child", "~> 1.0", true},
           {"hexpm", "other", "other", "~> 1.0", false}
         ]},
        {{:deps, "hexpm", "parent", "1.1.0"}, [{"hexpm", "child", "../child", "~> 1.0", false}]}
      ])

      %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
    end)

    assert {:ok, [child, other]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    assert child.constraint == %Hex.Solver.Constraints.Empty{}
    refute child.optional
    assert other.constraint == Hex.Solver.parse_constraint!("~> 1.0")

    assert_received {:mix_shell, :error,
                     [
                       "\e[33mPackage parent 1.0.0 can't be used because its dependency child " <>
                         "has the invalid application name \"../child\"" <> _
                     ]}

    assert {:ok, [_child, _other]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    refute_received {:mix_shell, :error, _}

    assert {:ok, [_child]} = Registry.dependencies("hexpm", "parent", "1.1.0")

    assert_received {:mix_shell, :error,
                     [
                       "\e[33mPackage parent 1.1.0 can't be used because its dependency child " <>
                         "has the invalid application name \"../child\"" <> _
                     ]}
  end

  test "versions/2 sorts the versions again after they change in the registry" do
    put_versions = fn versions ->
      :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
        :ets.insert(tid, {{:versions, "hexpm", "package"}, versions})
        %{state | fetched: MapSet.put(fetched, {"hexpm", "package"})}
      end)
    end

    put_versions.(["1.1.0", "1.0.0", "1.0.0-rc.1"])
    assert {:ok, versions} = Registry.versions("hexpm", "package")
    assert Enum.map(versions, &to_string/1) == ["1.0.0-rc.1", "1.0.0", "1.1.0"]
    assert {:ok, ^versions} = Registry.versions("hexpm", "package")

    put_versions.(["1.1.0", "2.0.0", "1.0.0"])
    assert {:ok, versions} = Registry.versions("hexpm", "package")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0", "2.0.0"]
  end

  test "persist/0 writes the cache only when it changed" do
    path = tmp_path("cache.ets")
    Registry.persist()
    File.rm!(path)

    Registry.persist()
    refute File.exists?(path)

    Registry.last_update(:calendar.universal_time())
    Registry.persist()
    assert File.exists?(path)
  end

  test "a version removed from the registry leaves no entries behind" do
    release = fn version ->
      %{
        version: version,
        inner_checksum: "inner",
        outer_checksum: "outer",
        retired: nil,
        published_at: %{seconds: 1_700_000_000, nanos: 0},
        advisory_indexes: [],
        dependencies: [%{package: "child", requirement: "~> 1.0"}]
      }
    end

    send_releases = fn releases ->
      send(Registry, {:get_package, "hexpm", "package", {:ok, {200, %{}, %{releases: releases}}}})
    end

    entries = fn version ->
      %{ets: tid} = :sys.get_state(Registry)
      for {{kind, "hexpm", "package", ^version}, _value} <- :ets.tab2list(tid), do: kind
    end

    send_releases.([release.("1.0.0"), release.("1.1.0")])

    assert Enum.sort(entries.("1.1.0")) ==
             ~w(advisories deps inner_checksum outer_checksum published_at retired timestamp)a

    send_releases.([release.("1.0.0")])
    assert entries.("1.1.0") == []
    assert Registry.outer_checksum("hexpm", "package", "1.1.0") == nil
    assert Registry.outer_checksum("hexpm", "package", "1.0.0") == "outer"
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

  describe "package etags" do
    setup do
      Registry.close()
      bypass = Bypass.open()
      repos = Hex.State.fetch!(:repos)
      Hex.State.put(:repos, put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}"))
      Hex.State.put(:offline, false)
      Hex.State.put(:shell_process, self())
      {:ok, bypass: bypass}
    end

    test "the etag of a fetched package is sent on the next fetch and a 304 keeps the cache",
         %{bypass: bypass} do
      test_pid = self()

      Bypass.expect(bypass, "GET", "/packages/etag_package", fn conn ->
        if_none_match = Plug.Conn.get_req_header(conn, "if-none-match")
        send(test_pid, {:if_none_match, if_none_match})

        case if_none_match do
          [] ->
            conn
            |> Plug.Conn.put_resp_header("etag", "\"v1\"")
            |> Plug.Conn.resp(200, signed_package("etag_package", ["1.0.0"]))

          ["\"v1\""] ->
            Plug.Conn.resp(conn, 304, "")
        end
      end)

      in_tmp("registry_package_etag", fn ->
        registry_path = Path.join(File.cwd!(), "cache.ets")
        Registry.open(check_version: false, registry_path: registry_path)

        Registry.prefetch([{"hexpm", "etag_package"}])
        assert {:ok, _versions} = Registry.versions("hexpm", "etag_package")
        assert_received {:if_none_match, []}

        Registry.close()
        Registry.open(check_version: false, registry_path: registry_path)

        Registry.prefetch([{"hexpm", "etag_package"}])
        assert {:ok, versions} = Registry.versions("hexpm", "etag_package")
        assert Enum.map(versions, &to_string/1) == ["1.0.0"]
        assert_received {:if_none_match, ["\"v1\""]}
        refute_received {:mix_shell, :error, ["Failed to fetch record" <> _]}
      end)
    end
  end

  test "dependencies/3 rejects dependency names the registry format does not allow" do
    deps = [
      {"hexpm", "child\e[2K\rFORGED", "child", ">= 0.0.0", false},
      {"hexpm", "Child", "child", ">= 0.0.0", false},
      {"hexpm", "1child", "child", ">= 0.0.0", false},
      {"hexpm", "", "child", ">= 0.0.0", false},
      {"hexpm", "child\u{202E}", "child", ">= 0.0.0", false},
      {"hexpm", "child-1", "child", ">= 0.0.0", false},
      {"hexpm", "child.1", "child", ">= 0.0.0", false}
    ]

    for {_repo, name, _app, _requirement, _optional} = dep <- deps do
      insert_deps([dep])

      error =
        assert_raise Mix.Error, fn -> Registry.dependencies("hexpm", "parent", "1.0.0") end

      assert error.message ==
               "Package parent 1.0.0 declares a dependency with an invalid package name " <>
                 inspect(name)
    end
  end

  test "dependencies/3 rejects repository names the registry format does not allow" do
    for repo <- ["acme\e[2K\rFORGED", "acme\u{202E}", "", "acme corp", "acme/corp"] do
      insert_deps([{repo, "child", "child", ">= 0.0.0", false}])

      error =
        assert_raise Mix.Error, fn -> Registry.dependencies("hexpm", "parent", "1.0.0") end

      assert error.message ==
               "Package parent 1.0.0 declares a dependency with an invalid repository name " <>
                 inspect(repo)
    end
  end

  test "dependencies/3 accepts registry and custom repository names" do
    insert_deps([
      {"hexpm", "child", "child", ">= 0.0.0", false},
      {"hexpm:acme", "child_2", "child_2", ">= 0.0.0", false},
      {"my-Repo.v2", "child3", "child3", ">= 0.0.0", false}
    ])

    assert {:ok, [hexpm, organization, custom]} =
             Registry.dependencies("hexpm", "parent", "1.0.0")

    assert hexpm.repo == nil
    assert organization.repo == "hexpm:acme"
    assert custom.repo == "my-Repo.v2"
  end

  defp insert_deps(deps) do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(tid, {{:deps, "hexpm", "parent", "1.0.0"}, deps})
      %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
    end)
  end

  test "prefetch raises a helpful error in offline mode when a package is not cached" do
    assert_raise Mix.Error,
                 ~r"Hex is running in offline mode and the registry entry for package missing_package is not cached locally",
                 fn ->
                   Registry.prefetch([{"hexpm", "missing_package"}])
                 end
  end

  defp signed_package(name, versions) do
    releases =
      Enum.map(versions, fn version ->
        %{
          version: version,
          inner_checksum: :crypto.hash(:sha256, "inner #{version}"),
          outer_checksum: :crypto.hash(:sha256, "outer #{version}"),
          dependencies: []
        }
      end)

    package = %{repository: "hexpm", name: name, releases: releases}
    :mix_hex_registry.build_package(package, File.read!(fixture_path("test_priv.pem")))
  end
end
