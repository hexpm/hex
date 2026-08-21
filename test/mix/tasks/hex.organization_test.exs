defmodule Mix.Tasks.Hex.OrganizationTest do
  use HexTest.IntegrationCase

  test "auth" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgauth", "orgauth@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])
      Hexpm.new_repo("myorgauth", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorgauth"])

      myorg = Hex.Repo.get_repo("hexpm:myorgauth")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo"
      assert is_binary(myorg.auth_key)

      {:ok, hostname} = :inet.gethostname()
      name = "#{hostname}-repository-myorgauth"
      assert {:ok, {200, _, body}} = Hex.API.Key.get(auth)
      assert name in Enum.map(body, & &1["name"])
    end)
  end

  test "auth without --key warns about deprecation" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgauthdeprecated", "orgauthdeprecated@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])
      Hexpm.new_repo("myorgauthdeprecated", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorgauthdeprecated"])

      output = Case.shell_output()
      assert output =~ "deprecated"
      assert output =~ "mix hex.user auth"

      # Still authorizes (warning only, no behavior change)
      myorg = Hex.Repo.get_repo("hexpm:myorgauthdeprecated")
      assert is_binary(myorg.auth_key)
      assert myorg.auth_key_owner == :user

      repos = Hex.Config.read_repos(Hex.Config.read())
      assert repos["hexpm:myorgauthdeprecated"].auth_key_owner == :user
    end)
  end

  test "auth with --keyname" do
    in_tmp(fn ->
      set_home_cwd()

      auth =
        Hexpm.new_user("orgauthwithkeyname", "orgauthwithkeyname@mail.com", "password", "key")

      Hex.State.put(:api_key, auth[:key])

      Hexpm.new_repo("myorgauthwithkeyname", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})

      Mix.Tasks.Hex.Organization.run([
        "auth",
        "myorgauthwithkeyname",
        "--key-name",
        "orgauthkeyname"
      ])

      myorg = Hex.Repo.get_repo("hexpm:myorgauthwithkeyname")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo"
      assert is_binary(myorg.auth_key)

      assert {:ok, {200, _, body}} = Hex.API.Key.get(auth)
      assert "orgauthkeyname-repository-myorgauthwithkeyname" in Enum.map(body, & &1["name"])
    end)
  end

  test "auth --key" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgauthkey", "orgauthkey@mail.com", "password", "key")
      Hexpm.new_repo("myorgauthkey", auth)

      parameters = [%{"domain" => "repository", "resource" => "myorgauthkey"}]
      {:ok, {201, _, body}} = Hex.API.Key.new("orgauthkey", parameters, auth)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgauthkey", "--key", body["secret"]])

      myorg = Hex.Repo.get_repo("hexpm:myorgauthkey")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo"
      assert myorg.auth_key == body["secret"]

      repos = Hex.Config.read_repos(Hex.Config.read())
      assert repo = repos["hexpm:myorgauthkey"]
      assert repo[:auth_key]
      assert repo[:trusted]
      assert repo[:url] == "http://localhost:4043/repo"

      refute Map.has_key?(Hex.Config.read()[:"$repos"]["hexpm:myorgauthkey"], :trusted)
    end)
  end

  test "auth --key with an organization-owned key records the owner" do
    in_tmp(fn ->
      set_home_cwd()
      bypass = Bypass.open()
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      body = %{
        "key" => %{
          "name" => "ci",
          "owner" => %{"type" => "organization", "name" => "myorgownedkey"}
        }
      }

      Bypass.expect(bypass, "GET", "/api/auth", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(body))
      end)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgownedkey", "--key", "orgkeysecret"])

      output = Case.shell_output()

      assert output =~
               "Starting with Hex 2.6 this command will exchange the organization key for a short-lived token"

      refute output =~ "deprecated"

      repos = Hex.Config.read_repos(Hex.Config.read())
      assert repos["hexpm:myorgownedkey"].auth_key == "orgkeysecret"
      assert repos["hexpm:myorgownedkey"].auth_key_owner == :organization
    end)
  end

  test "auth --key with a user-owned key warns about deprecation" do
    in_tmp(fn ->
      set_home_cwd()
      bypass = Bypass.open()
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      body = %{
        "key" => %{
          "name" => "laptop",
          "owner" => %{"type" => "user", "name" => "eric"}
        }
      }

      Bypass.expect(bypass, "GET", "/api/auth", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(body))
      end)

      Mix.Tasks.Hex.Organization.run(["auth", "myuserownedkey", "--key", "userkeysecret"])

      output = Case.shell_output()
      assert output =~ "The given key is owned by your user account, not the organization"
      assert output =~ "will stop working in Hex 2.6"
      assert output =~ "mix hex.organization key myuserownedkey generate"
      refute output =~ "Starting with Hex 2.6"

      # Still authorizes (warning only, no behavior change)
      repos = Hex.Config.read_repos(Hex.Config.read())
      assert repos["hexpm:myuserownedkey"].auth_key == "userkeysecret"
      assert repos["hexpm:myuserownedkey"].auth_key_owner == :user
    end)
  end

  test "auth --key against a server without key owner information stores no owner" do
    in_tmp(fn ->
      set_home_cwd()
      bypass = Bypass.open()
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      Bypass.expect(bypass, "GET", "/api/auth", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgnoowner", "--key", "noownersecret"])

      output = Case.shell_output()
      refute output =~ "owned by your user account"
      refute output =~ "Starting with Hex 2.6"

      repos = Hex.Config.read_repos(Hex.Config.read())
      assert repos["hexpm:myorgnoowner"].auth_key == "noownersecret"
      refute Map.has_key?(repos["hexpm:myorgnoowner"], :auth_key_owner)
    end)
  end

  test "auth --key with invalid key" do
    in_tmp(fn ->
      set_home_cwd()

      assert catch_throw(Mix.Tasks.Hex.Organization.run(["auth", "myorg", "--key", "mykey"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :error,
                       [
                         "Failed to authenticate against organization repository with given key because of: invalid API key"
                       ]}
    end)
  end

  test "deauth" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgdeauth", "orgdeauth@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])
      Hexpm.new_repo("myorgdeauth", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorgdeauth"])

      Mix.Tasks.Hex.Organization.run(["deauth", "myorgdeauth"])
      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:myorgdeauth"]
    end)
  end

  test "key --generate" do
    in_tmp(fn ->
      set_home_cwd()

      auth = Hexpm.new_user("orgkeygenuser", "orgkeygenuser@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])

      Hexpm.new_repo("orgkeygenrepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeygenrepo", "generate", "--permission", "api:read"]
      Mix.Tasks.Hex.Organization.run(args)

      assert_received {:mix_shell, :info, [key]}
      assert is_binary(key)

      {:ok, hostname} = :inet.gethostname()
      assert {:ok, {200, _, body}} = Hex.API.Key.Organization.get("orgkeygenrepo", key: key)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, _, body}} = Hex.API.Key.Organization.get("orgkeygenrepo", auth)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, _, body}} = Hex.API.Key.get(auth)
      refute List.to_string(hostname) in Enum.map(body, & &1["name"])
    end)
  end

  test "list keys" do
    in_tmp(fn ->
      set_home_cwd()

      auth = Hexpm.new_user("orgkeylistuser", "orgkeylistuser@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])

      Hexpm.new_repo("orgkeylistrepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeylistrepo", "generate", "--key-name", "orgkeylistrepo"]
      Mix.Tasks.Hex.Organization.run(args)

      assert {:ok, {200, _, [%{"name" => "orgkeylistrepo"}]}} =
               Hex.API.Key.Organization.get("orgkeylistrepo", auth)

      Mix.Tasks.Hex.Organization.run(["key", "orgkeylistrepo", "list"])
      assert_received {:mix_shell, :info, ["orgkeylistrepo" <> _]}
    end)
  end

  test "revoke key" do
    in_tmp(fn ->
      set_home_cwd()

      auth = Hexpm.new_user("orgkeyrevokeyuser", "orgkeyrevokeyuser@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])

      Hexpm.new_repo("orgkeyrevokerepo", auth)
      Hexpm.new_organization_key("orgkeyrevokerepo", "orgkeyrevokerepo2", auth)

      assert {:ok, {200, _, [%{"name" => "orgkeyrevokerepo2"}]}} =
               Hex.API.Key.Organization.get("orgkeyrevokerepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevokerepo", "revoke", "orgkeyrevokerepo2"])
      assert_received {:mix_shell, :info, ["Revoking key orgkeyrevokerepo2..."]}

      assert {:ok, {200, _, []}} = Hex.API.Key.Organization.get("orgkeyrevokerepo", auth)
    end)
  end

  test "revoke all keys" do
    in_tmp(fn ->
      set_home_cwd()

      auth =
        Hexpm.new_user("orgkeyrevokealluser", "orgkeyrevokealluser@mail.com", "password", "key")

      Hex.State.put(:api_key, auth[:key])

      Hexpm.new_repo("orgkeyrevokeallrepo", auth)
      Hexpm.new_organization_key("orgkeyrevokeallrepo", "orgkeyrevokeallrepo2", auth)

      assert {:ok, {200, _, [%{"name" => "orgkeyrevokeallrepo2"}]}} =
               Hex.API.Key.Organization.get("orgkeyrevokeallrepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevokeallrepo", "revoke", "--all"])
      assert_received {:mix_shell, :info, ["Revoking all keys..."]}

      assert {:ok, {200, _, []}} = Hex.API.Key.Organization.get("orgkeyrevokeallrepo", auth)
    end)
  end
end
