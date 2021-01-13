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

  @tag skip: System.version() < "1.3" and System.otp_release() < "19"
  test "find_config_home/1 when MIX_XDG is set and HEX_HOME is not" do
    System.delete_env("HEX_HOME")
    System.put_env("MIX_XDG", "true")
    {:ok, dir} = Config.find_config_home(:user_cache)
    assert dir =~ "/hex"
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
