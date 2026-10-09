defmodule Hex.SCMIntegrationTest do
  use HexTest.IntegrationCase

  setup do
    Hex.Registry.Server.open()
    Hex.Registry.Server.prefetch([{"hexpm", "postgrex"}])
  end

  test "fetch downloads the tarball into the cache" do
    path = Hex.SCM.cache_path("hexpm", "postgrex", "0.2.1")
    File.rm(path)

    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") == {:ok, :new}
    outer_checksum = Hex.Registry.Server.outer_checksum("hexpm", "postgrex", "0.2.1")
    assert Hex.Tar.outer_checksum(path) == {:ok, outer_checksum}
    assert Path.wildcard(path <> ".*") == []

    assert Hex.SCM.fetch("hexpm", "postgrex", "0.2.1") == {:ok, :cached}
  end

  test "fetch leaves nothing in the cache when the download fails" do
    path = Hex.SCM.cache_path("hexpm", "postgrex", "9.9.9")

    assert Hex.SCM.fetch("hexpm", "postgrex", "9.9.9") == {:error, "Request failed (404)"}
    refute File.exists?(path)
    assert Path.wildcard(path <> ".*") == []
  end
end
