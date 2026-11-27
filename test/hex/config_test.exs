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

  test "read/0 raises when the config is not readable" do
    in_tmp(fn ->
      set_home_cwd()

      File.write!(Path.join(Hex.State.fetch!(:config_home), "hex.config"), "[foo: :bar]")

      assert_raise Mix.Error, ~r/remove the file and run `mix hex\.user auth`/, fn ->
        Config.read()
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

    test "write/1 tightens a directory an older Hex already created" do
      in_tmp(fn ->
        home = Path.join(File.cwd!(), "hex_home")
        set_home_path(home)
        File.mkdir_p!(home)
        File.chmod!(home, 0o755)

        Config.write("$oauth_token": %{access_token: "a_token"})

        assert permissions(home) == 0o700
      end)
    end

    test "write/1 replaces the config instead of writing the tokens into it in place" do
      in_tmp(fn ->
        set_home_cwd()
        Config.write("$oauth_token": %{access_token: "a_token"})
        inode = File.stat!("hex.config").inode

        Config.write("$oauth_token": %{access_token: "b_token"})

        assert File.stat!("hex.config").inode != inode
        assert permissions("hex.config") == 0o600
        assert Path.wildcard("*") == ["hex.config"]
      end)
    end

    defp permissions(path) do
      rem(File.stat!(path).mode, 0o1000)
    end
  end

  test "update_repo/2 keeps concurrent repository writes from dropping each other" do
    in_tmp(fn ->
      set_home_cwd()
      Config.write([])

      names = Enum.map(1..15, &"hexpm:org#{&1}")

      names
      |> Enum.map(fn name ->
        Task.async(fn -> Config.update_repo(name, &Map.put(&1, :auth_key, name)) end)
      end)
      |> Task.await_many(10_000)

      repos = Config.read()[:"$repos"]

      for name <- names do
        assert repos[name].auth_key == name
      end
    end)
  end

  test "read/0 raises when a stored binary is not valid UTF-8" do
    in_tmp(fn ->
      set_home_cwd()

      Config.write(foo: [<<0xFF, 0xFE>>])

      assert_raise Mix.Error, ~r/remove the file and run `mix hex\.user auth`/, fn ->
        Config.read()
      end
    end)
  end

  test "update_repo/2 keeps a repository URL that only memory has" do
    in_tmp(fn ->
      set_home_cwd()
      Config.write([])

      # HEX_MIRROR and HEX_REPOS both land here without ever reaching the file,
      # so deriving the new state from the file would send the next request to
      # whatever the file says instead.
      repos = put_in(Hex.State.fetch!(:repos)["hexpm"].url, "http://localhost:4043/repo")
      Hex.State.put(:repos, repos)

      Config.update_repo("hexpm", &Map.put(&1, :oauth_token, %{access_token: "a_token"}))

      assert Hex.State.fetch!(:repos)["hexpm"].url == "http://localhost:4043/repo"
      assert Hex.State.fetch!(:repos)["hexpm"].oauth_token == %{access_token: "a_token"}
      assert Config.read()[:"$repos"]["hexpm"].oauth_token == %{access_token: "a_token"}
    end)
  end

  test "update_repo/2 keeps a repository the file has and memory does not" do
    in_tmp(fn ->
      set_home_cwd()
      Config.write("$repos": %{"hexpm:other" => %{auth_key: "other_key"}})

      Config.update_repo("hexpm", &Map.put(&1, :oauth_token, %{access_token: "a_token"}))

      assert Config.read()[:"$repos"]["hexpm:other"].auth_key == "other_key"
    end)
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

  test "read/0 drops the repository with an empty name" do
    in_tmp(fn ->
      set_home_cwd()

      Config.write(
        "$repos": %{
          "" => %{url: "https://repo.hex.pm", auth_key: "legacy_key", oauth_exchange: true},
          "acme" => %{url: "https://acme.example"}
        }
      )

      assert Map.keys(Config.read()[:"$repos"]) == ["acme"]
    end)
  end

  test "read/0 drops the hexpm key when there is a user session" do
    in_tmp(fn ->
      set_home_cwd()

      repos = %{
        "hexpm" => %{auth_key: "legacy_key", oauth_token: %{access_token: "t", expires_at: 1}},
        "acme" => %{url: "https://acme.example", auth_key: "acme_key"}
      }

      Config.write("$repos": repos)
      assert Config.read()[:"$repos"]["hexpm"].auth_key == "legacy_key"

      session = %{access_token: "a_token", refresh_token: "r_token", expires_at: 123}
      Config.write("$oauth_token": session, "$repos": repos)
      config = Config.read()

      assert config[:"$repos"]["hexpm"].auth_key == nil
      refute Map.has_key?(config[:"$repos"]["hexpm"], :oauth_token)
      assert config[:"$repos"]["acme"].auth_key == "acme_key"
      assert config[:"$oauth_token"] == session
    end)
  end

  test "read/0 keeps the key of a hexpm repository with another URL when there is a user session" do
    in_tmp(fn ->
      set_home_cwd()

      session = %{access_token: "a_token", refresh_token: "r_token", expires_at: 123}

      Config.write(
        "$oauth_token": session,
        "$repos": %{"hexpm" => %{url: "https://mirror.example", auth_key: "mirror_key"}}
      )

      assert Config.read()[:"$repos"]["hexpm"].auth_key == "mirror_key"

      Config.write(
        "$oauth_token": session,
        "$repos": %{"hexpm" => %{url: "https://repo.hex.pm/", auth_key: "legacy_key"}}
      )

      assert Config.read()[:"$repos"]["hexpm"].auth_key == nil
    end)
  end
end
