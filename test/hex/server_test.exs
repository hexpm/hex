defmodule Hex.ServerTest do
  use ExUnit.Case, async: true

  test "should_warn_lock_version?/0" do
    {:ok, pid} = Hex.Server.start_link(name: nil)
    assert Hex.Server.should_warn_lock_version?(pid)
    refute Hex.Server.should_warn_lock_version?(pid)
  end

  test "should_warn_registry_version?/0" do
    {:ok, pid} = Hex.Server.start_link(name: nil)
    assert Hex.Server.should_warn_registry_version?(pid)
    refute Hex.Server.should_warn_registry_version?(pid)
  end

  test "should_warn?/1 returns true only once per key" do
    {:ok, pid} = Hex.Server.start_link(name: nil)
    assert Hex.Server.should_warn?(:one, pid)
    refute Hex.Server.should_warn?(:one, pid)
    assert Hex.Server.should_warn?(:two, pid)
    refute Hex.Server.should_warn?(:two, pid)
  end

  test "should_warn?/1 tracks keys independently" do
    {:ok, pid} = Hex.Server.start_link(name: nil)
    assert Hex.Server.should_warn?({:repo, "acme"}, pid)
    assert Hex.Server.should_warn?({:repo, "other"}, pid)
    refute Hex.Server.should_warn?({:repo, "acme"}, pid)
  end
end
