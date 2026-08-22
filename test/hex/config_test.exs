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

  test "update/1 keeps concurrent writes from dropping each other" do
    in_tmp(fn ->
      set_home_cwd()
      Config.write([])

      keys = Enum.map(1..25, &:"key_#{&1}")

      keys
      |> Enum.map(fn key -> Task.async(fn -> Config.update([{key, Atom.to_string(key)}]) end) end)
      |> Task.await_many(10_000)

      config = Config.read()

      for key <- keys do
        assert config[key] == Atom.to_string(key)
      end
    end)
  end

  if match?({:unix, _}, :os.type()) do
    test "write/1 leaves the config readable only by its owner" do
      in_tmp(fn ->
        home = Path.join(File.cwd!(), "hex_home")
        set_home_path(home)

        Config.write("$oauth_token": %{access_token: "a_token", refresh_token: "r_token"})

        assert permissions(Path.join(home, "hex.config")) == 0o600
        assert permissions(home) == 0o700
      end)
    end

    test "write/1 tightens the permissions of a config written by an older Hex" do
      in_tmp(fn ->
        set_home_cwd()
        Config.write([])
        File.chmod!("hex.config", 0o644)

        Config.write("$oauth_token": %{access_token: "a_token"})

        assert permissions("hex.config") == 0o600
      end)
    end

    defp permissions(path) do
      rem(File.stat!(path).mode, 0o1000)
    end
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
