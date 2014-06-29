defmodule Hex.RegistryTest do
  use HexTest.Case

  test "stat" do
    Hex.Registry.start(registry_path: tmp_path("hex.ets"))

    assert Hex.Registry.stat == {10, 31}
  end

  test "install info output once" do
    in_tmp fn ->
      Application.put_env(:hex, :registry_updated, false)
      System.put_env("MIX_HOME", System.cwd!)

      path = "hex.ets"
      versions = [{"100.0.0", "0.0.1"}]
      create_registry(path, 2, versions, [], [])

      Hex.Registry.start
      Hex.Util.update_registry(no_fetch: true)
      assert_received {:mix_shell, :error, ["A new Hex version is available" <> _]}

      Hex.Util.update_registry(no_fetch: true)
      refute_received {:mix_shell, :error, ["A new Hex version is available" <> _]}
    end
  end

  test "install info, find correct version" do
    in_tmp fn ->
      Application.put_env(:hex, :registry_updated, false)
      System.put_env("MIX_HOME", System.cwd!)

      path = "hex.ets"
      versions = [{"100.0.0", "100.0.0"}, {"0.0.1", "0.0.1"}, {"99.0.0", "0.0.1"}, {"100.0.0", "0.0.1"}, {"98.0.0", "0.0.1"}]
      create_registry(path, 2, versions, [], [])

      Hex.Registry.start
      Hex.Util.update_registry(no_fetch: true)
      assert_received {:mix_shell, :error, ["A new Hex version is available (v100.0.0), please update with `mix local.hex`"]}
    end
  end

  test "install info, too new elixir" do
    in_tmp fn ->
      Application.put_env(:hex, :registry_updated, false)
      System.put_env("MIX_HOME", System.cwd!)

      path = "hex.ets"
      versions = [{"100.0.0", "100.0.0"}]
      create_registry(path, 2, versions, [], [])

      Hex.Registry.start
      Hex.Util.update_registry(no_fetch: true)
      refute_received {:mix_shell, :error, ["A new Hex version is available" <> _]}
    end
  end

  test "install info, too old hex" do
    in_tmp fn ->
      Application.put_env(:hex, :registry_updated, false)
      System.put_env("MIX_HOME", System.cwd!)

      path = "hex.ets"
      versions = [{"0.0.1", "0.0.1"}]
      create_registry(path, 2, versions, [], [])

      Hex.Registry.start
      Hex.Util.update_registry(no_fetch: true)
      refute_received {:mix_shell, :error, ["A new Hex version is available" <> _]}
    end
  end
end
