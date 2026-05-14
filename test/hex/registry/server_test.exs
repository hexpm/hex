defmodule Hex.Registry.ServerTest do
  use HexTest.Case
  alias Hex.Registry.Server, as: Registry

  setup do
    Hex.State.put(:offline, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    :ok
  end

  test "prefetch raises a helpful error in offline mode when a package is not cached" do
    assert_raise Mix.Error,
                 ~r"Hex is running in offline mode and the registry entry for package missing_package is not cached locally",
                 fn ->
                   Registry.prefetch([{"hexpm", "missing_package"}])
                 end
  end
end
