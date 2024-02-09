defmodule Hex.ConfigTest do
  use ExUnit.Case
  alias Hex.Config

  test "find_config_home/1 when no env var flags are set" do
    System.delete_env("HEX_HOME")
    {:ok, dir} = Config.find_config_home(:user_data)
    assert dir =~ ".hex"
  end

  test "find_config_home/1 when HEX_HOME is set" do
    System.put_env("HEX_HOME", "/sys/tmp")
    assert Config.find_config_home(:user_cache) == {{:env, "HEX_HOME"}, "/sys/tmp"}
  after
    System.delete_env("HEX_HOME")
  end

  test "find_config_home/1 when MIX_XDG is set and HEX_HOME is not" do
    System.delete_env("HEX_HOME")
    System.put_env("MIX_XDG", "true")
    System.put_env("XDG_CONFIG_HOME", "/xdg_config_home")
    System.put_env("XDG_CACHE_HOME", "/xdg_cache_home")

    assert {{:env, "MIX_XDG"}, "/xdg_config_home/hex"} = Config.find_config_home(:user_config)
    assert {{:env, "MIX_XDG"}, "/xdg_cache_home/hex"} = Config.find_config_home(:user_cache)
  after
    System.delete_env("MIX_XDG")
    System.delete_env("XDG_CONFIG_HOME")
    System.delete_env("XDG_CACHE_HOME")
  end

  test "find_config_home/1 when MIX_XDG is set and HEX_HOME is set" do
    System.put_env("MIX_XDG", "true")
    System.put_env("HEX_HOME", "/sys/tmp")

    assert Config.find_config_home(:user_cache) == {{:env, "HEX_HOME"}, "/sys/tmp"}
  after
    System.delete_env("HEX_HOME")
    System.delete_env("MIX_XDG")
  end
end
