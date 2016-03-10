defmodule Hex.RegistryTest do
  use HexTest.Case

  test "stat" do
    Hex.PackageRegistry.open!(registry_path: tmp_path("registry.ets"))
    assert Hex.PackageRegistry.stat == {18, 46}
    assert Hex.PackageRegistry.close == true

    # Multiple open and close should yield the same result
    Hex.PackageRegistry.open!(registry_path: tmp_path("registry.ets"))
    assert Hex.PackageRegistry.stat == {18, 46}
    assert Hex.PackageRegistry.close == true
  end

  test "install info output once" do
    in_tmp fn ->
      Hex.State.put(:registry_updated, false)
      Hex.State.put(:home, System.cwd!)

      path = "registry.ets"
      versions = [{"100.0.0", ["0.0.1"]}]
      create_registry(path, 3, versions, [], [])

      Hex.PackageRegistry.open!
      Hex.Utils.ensure_registry!(fetch: false)
      assert_received {:mix_shell, :info, ["\e[33mA new Hex version is available" <> _]}

      Hex.Utils.ensure_registry!(fetch: false)
      refute_received {:mix_shell, :info, ["\e[33mA new Hex version is available" <> _]}
    end
  end

  test "install info, find correct version" do
    in_tmp fn ->
      Hex.State.put(:registry_updated, false)
      Hex.State.put(:home, System.cwd!)

      path = "registry.ets"
      versions = [{"100.0.0", ["100.0.0"]}, {"0.0.1", ["0.0.1"]}, {"99.0.0", ["0.0.1"]},
                  {"100.0.0", ["0.0.1"]}, {"98.0.0", ["0.0.1"]}]
      create_registry(path, 3, versions, [], [])

      Hex.PackageRegistry.open!
      Hex.Utils.ensure_registry!(fetch: false)
      assert_received {:mix_shell, :info, ["\e[33mA new Hex version is available (100.0.0), please update with `mix local.hex`\e[0m"]}
    end
  end

  test "install info, too new elixir" do
    in_tmp fn ->
      Hex.State.put(:registry_updated, false)
      Hex.State.put(:home, System.cwd!)

      path = "registry.ets"
      versions = [{"100.0.0", ["100.0.0"]}]
      create_registry(path, 3, versions, [], [])

      Hex.PackageRegistry.open!
      Hex.Utils.ensure_registry!(fetch: false)
      refute_received {:mix_shell, :info, ["A new Hex version is available" <> _]}
    end
  end

  test "install info, too old hex" do
    in_tmp fn ->
      Hex.State.put(:registry_updated, false)
      Hex.State.put(:home, System.cwd!)

      path = "registry.ets"
      versions = [{"0.0.1", ["0.0.1"]}]
      create_registry(path, 3, versions, [], [])

      Hex.PackageRegistry.open!
      Hex.Utils.ensure_registry!(fetch: false)
      refute_received {:mix_shell, :info, ["A new Hex version is available" <> _]}
    end
  end
end
