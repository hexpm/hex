defmodule Hex.ConfigTest do
  use ExUnit.Case, async: false
  alias Hex.Config

  test "find_config_home/1 when no env var flags are set" do
    System.delete_env("HEX_HOME")
    {:ok, dir} = Config.find_config_home(:user_data)
    assert dir =~ ".hex"
  end

  test "find_config_home/1 when HEX_HOME is set" do
    System.put_env("HEX_HOME", "/sys/tmp")
    assert Config.find_config_home(:user_cache) == {:ok, "/sys/tmp"}
    System.delete_env("HEX_HOME")
  end

  @tag skip: Version.match?(System.version(), "< 1.6.0")
  test "find_config_home/1 when MIX_XDG is set and HEX_HOME is not" do
    System.delete_env("HEX_HOME")
    System.put_env("MIX_XDG", "true")
    {:ok, cache_dir} = Config.find_config_home(:user_cache)
    {:ok, config_dir} = Config.find_config_home(:user_config)

    case :os.type() do
      {_, :linux} ->
        assert config_dir =~ ".config/hex"
        assert cache_dir =~ ".cache/hex"

      {_, :darwin} ->
        assert config_dir =~ "Application Support/hex"
        assert cache_dir =~ "Caches/hex"
    end

    System.delete_env("MIX_XDG")
  end

  test "find_config_home/1 when MIX_XDG is set and HEX_HOME is set" do
    System.put_env("MIX_XDG", "true")
    System.put_env("HEX_HOME", "/sys/tmp")
    assert Config.find_config_home(:user_cache) == {:ok, "/sys/tmp"}
    System.delete_env("HEX_HOME")
    System.delete_env("MIX_XDG")
  end
end
