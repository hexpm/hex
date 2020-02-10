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
end
