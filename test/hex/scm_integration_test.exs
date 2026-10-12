defmodule Hex.SCMIntegrationTest do
  use HexTest.IntegrationCase

  defmodule Project do
    def project, do: [app: :scm_integration, version: "0.1.0", deps: []]
  end

  setup do
    Hex.Registry.Server.open()
    Hex.Registry.Server.prefetch([{"hexpm", "postgrex"}])
  end

  test "fetch leaves nothing in the cache when the checksum doesn't match the registry" do
    path = Hex.SCM.cache_path("hexpm", "postgrex", "0.2.1")
    File.rm(path)

    key = {:outer_checksum, "hexpm", "postgrex", "0.2.1"}
    outer_checksum = Hex.Registry.Server.outer_checksum("hexpm", "postgrex", "0.2.1")
    put_registry_entry(key, <<0::256>>)

    try do
      assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") ==
               {:error, "Checksum mismatch against registry (outer)"}

      refute File.exists?(path)
      assert Path.wildcard(path <> ".*") == []
    after
      put_registry_entry(key, outer_checksum)
    end
  end

  test "fetch removes a cached tarball that doesn't match the registry when the download fails" do
    path = Hex.SCM.cache_path("hexpm", "postgrex", "0.2.1")
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "not a tarball")
    mirror(&Plug.Conn.resp(&1, 500, ""))

    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") == {:error, "Request failed (500)"}
    refute File.exists?(path)
  end

  test "fetch doesn't use a cached tarball that doesn't match the registry on a 304" do
    path = Hex.SCM.cache_path("hexpm", "postgrex", "0.2.1")
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "not a tarball")
    mirror(&Plug.Conn.resp(&1, 304, ""))

    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") == {:error, "Request failed (304)"}
    refute File.exists?(path)
  end

  test "update doesn't unpack a cached tarball that doesn't match the registry when the fetch fails" do
    Mix.Project.push(Project)

    in_tmp(fn ->
      path = Hex.SCM.cache_path("hexpm", "postgrex", "0.2.1")
      File.mkdir_p!(Path.dirname(path))
      File.rm(path)

      # Another fetch caches a tarball that doesn't match while this one fails
      mirror(fn conn ->
        File.write!(path, "not a tarball")
        Plug.Conn.resp(conn, 500, "")
      end)

      assert_raise Mix.Error, ~r"Package fetch failed and no cached copy available", fn ->
        update_postgrex()
      end

      refute File.exists?(path)
      refute File.exists?("deps/postgrex")
    end)
  end

  test "fetch in offline mode only uses a cached tarball that matches the registry" do
    path = Hex.SCM.cache_path("hexpm", "postgrex", "0.2.1")
    File.rm(path)
    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") == {:ok, :new}

    Hex.State.put(:offline, true)
    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") == {:ok, :offline}

    File.write!(path, "not a tarball")

    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") ==
             {:error,
              "Hex is running in offline mode and the cached package for postgrex 0.2.1 " <>
                "doesn't match the registry checksum"}

    refute File.exists?(path)

    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") ==
             {:error,
              "Hex is running in offline mode and the package postgrex 0.2.1 is not cached locally"}
  end

  test "update in offline mode doesn't unpack a cached tarball that doesn't match the registry" do
    Mix.Project.push(Project)

    in_tmp(fn ->
      path = Hex.SCM.cache_path("hexpm", "postgrex", "0.2.1")
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, "not a tarball")
      Hex.State.put(:offline, true)

      assert_raise Mix.Error, ~r"doesn't match the registry checksum", fn ->
        update_postgrex()
      end

      refute File.exists?(path)
      refute File.exists?("deps/postgrex")
    end)
  end

  test "fetch reports a cached version that the registry doesn't list" do
    path = Hex.SCM.cache_path("hexpm", "postgrex", "9.9.9")
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "not a tarball")

    assert Hex.SCM.fetch("hexpm", "postgrex", "9.9.9") == {:error, "Request failed (404)"}
    refute File.exists?(path)
  end

  defp mirror(fun) do
    # Waits for the registry to be fetched before the repository changes
    Hex.Registry.Server.outer_checksum("hexpm", "postgrex", "0.2.1")
    bypass = Bypass.open()
    Hex.State.update!(:repos, &put_in(&1["hexpm"].url, "http://localhost:#{bypass.port}"))
    Bypass.expect_once(bypass, "GET", "/tarballs/postgrex-0.2.1.tar", fun)
  end

  defp update_postgrex() do
    inner_checksum = Hex.Registry.Server.inner_checksum("hexpm", "postgrex", "0.2.1")
    outer_checksum = Hex.Registry.Server.outer_checksum("hexpm", "postgrex", "0.2.1")

    lock =
      {:hex, :postgrex, "0.2.1", Base.encode16(inner_checksum, case: :lower), [:mix], [], "hexpm",
       Base.encode16(outer_checksum, case: :lower)}

    Hex.SCM.prefetch(%{postgrex: lock})
    Hex.SCM.update(hex: "postgrex", repo: "hexpm", dest: Path.expand("deps/postgrex"), lock: lock)
  end

  defp put_registry_entry(key, value) do
    :sys.replace_state(Hex.Registry.Server, fn state ->
      :ets.insert(state.ets, {key, value})
      state
    end)
  end
end
