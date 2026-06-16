defmodule Hex.ConfigTest do
  use HexTest.Case
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

  test "read/0 migrates string-keyed OAuth tokens to atom keys" do
    in_tmp(fn ->
      set_home_cwd()

      Config.write(
        "$oauth_token": %{
          "access_token" => "a_token",
          "refresh_token" => "r_token",
          "expires_at" => 123
        },
        "$repos": %{
          "hexpm:org" => %{
            url: "https://example.com",
            oauth_token: %{
              "access_token" => "repo_token",
              "expires_at" => 456
            }
          }
        }
      )

      config = Config.read()

      assert config[:"$oauth_token"] == %{
               access_token: "a_token",
               refresh_token: "r_token",
               expires_at: 123
             }

      assert config[:"$repos"]["hexpm:org"].oauth_token == %{
               access_token: "repo_token",
               expires_at: 456
             }
    end)
  end
end
