defmodule Hex.ConfigTest do
  use ExUnit.Case, async: false
  alias Hex.Config

  test "find_config_home/1 when no env var flags are set" do
    {:ok, dir} = Config.find_config_home(:user_data)
    assert String.match?(dir, ~r/\/.hex/)
  end

  test "find_config_home/1 when HEX_HOME is set" do
    System.put_env("HEX_HOME", "/sys/tmp")
    assert Config.find_config_home(:user_cache) == {:ok, "/sys/tmp"}
    System.delete_env("HEX_HOME")
  end
end
