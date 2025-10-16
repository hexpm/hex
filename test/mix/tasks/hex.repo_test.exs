defmodule Mix.Tasks.Hex.RepoTest do
  use HexTest.Case

  @public_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
  Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
  IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
  3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
  XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
  J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
  0wIDAQAB
  -----END PUBLIC KEY-----
  """

  @public_key_fingerprint "SHA256:O1LOYhHFW4kcrblKAxROaDEzLD8bn1seWbe5tq8TRsk"

  defp bypass_public_key() do
    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)
    repos = put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}")
    Hex.State.put(:repos, repos)

    Bypass.expect(bypass, fn %Plug.Conn{request_path: path} = conn ->
      case path do
        "/public_key" ->
          Plug.Conn.resp(conn, 200, @public_key)

        "/not_found/public_key" ->
          Plug.Conn.resp(conn, 404, "not found")
      end
    end)

    bypass
  end

  defp bypass_endpoint_url(bypass) do
    "http://localhost:#{bypass.port}"
  end

  test "add" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      File.write!("public_key.pem", @public_key)
      Mix.Tasks.Hex.Repo.run(["add", "reponame", "url", "--public-key", "public_key.pem"])

      assert [
               "$repos": %{
                 "reponame" => %{
                   auth_key: nil,
                   public_key: "-----BEGIN PUBLIC KEY" <> _,
                   url: "url"
                 }
               }
             ] = Hex.Config.read()

      File.write!("foo.pem", "INVALID PUBLIC KEY")

      assert_raise Mix.Error, fn ->
        Mix.Tasks.Hex.Repo.run(["add", "reponame", "url", "--public-key", "foo.pem"])
      end

      bypass = bypass_public_key()
      bypass_endpoint = bypass_endpoint_url(bypass)

      Mix.Tasks.Hex.Repo.run([
        "add",
        "another-reponame",
        bypass_endpoint,
        "--fetch-public-key",
        @public_key_fingerprint,
        "--auth-key",
        "AAAA",
        "--no-oauth-exchange"
      ])

      assert [
               "$repos": %{
                 "another-reponame" => %{
                   auth_key: "AAAA",
                   public_key: "-----BEGIN PUBLIC KEY" <> _,
                   url: ^bypass_endpoint
                 }
               }
             ] = Hex.Config.read()

      assert_raise Mix.Error, ~r/Public key fingerprint mismatch/, fn ->
        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame-wrong-fingerprint",
          bypass_endpoint,
          "--fetch-public-key",
          "WRONG-FINGERPRINT"
        ])
      end

      refute Keyword.get(Hex.Config.read(), :"$repos")["reponame-wrong-fingerprint"]

      assert catch_throw(
               Mix.Tasks.Hex.Repo.run([
                 "add",
                 "not-found-reponame",
                 "#{bypass_endpoint}/not_found",
                 "--fetch-public-key",
                 @public_key_fingerprint
               ])
             ) == {:exit_code, 1}

      assert_received {:mix_shell, :error, ["Downloading public key failed with code \"404\""]}

      Bypass.down(bypass)

      assert catch_throw(
               Mix.Tasks.Hex.Repo.run([
                 "add",
                 "reponame-connection-error",
                 bypass_endpoint,
                 "--fetch-public-key",
                 @public_key_fingerprint
               ])
             ) == {:exit_code, 1}

      assert_received {:mix_shell, :error, ["Downloading public key failed"]}
    end)
  end

  test "remove" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      Mix.Tasks.Hex.Repo.run(["add", "reponame", "url"])
      Mix.Tasks.Hex.Repo.run(["remove", "reponame"])
      assert ["$repos": %{}] = Hex.Config.read()
    end)
  end

  test "set url" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      Mix.Tasks.Hex.Repo.run(["add", "reponame", "url"])
      Mix.Tasks.Hex.Repo.run(["set", "reponame", "--url", "other_url"])
      assert ["$repos": %{"reponame" => %{url: "other_url"}}] = Hex.Config.read()
    end)
  end

  test "show url" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      Mix.Tasks.Hex.Repo.run(["add", "reponame", "url"])
      Mix.Tasks.Hex.Repo.run(["show", "reponame", "--url"])

      assert_received {:mix_shell, :info, ["url"]}
    end)
  end

  test "show prints repo config" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      Mix.Tasks.Hex.Repo.run(["add", "reponame", "url"])
      Mix.Tasks.Hex.Repo.run(["show", "reponame"])
      assert_received {:mix_shell, :info, [headers]}
      assert_received {:mix_shell, :info, [config]}
      assert headers =~ ~r{URL.*Public key.*Auth key}
      assert config =~ "url"
    end)
  end

  test "show raises an error when called with non-existant repo name" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      assert_raise Mix.Error, "Config does not contain repo non-existant-reponame", fn ->
        Mix.Tasks.Hex.Repo.run(["show", "non-existant-reponame"])
      end
    end)
  end

  describe "OAuth exchange configuration" do
    test "add with --no-oauth-exchange disables OAuth token exchange" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame",
          "http://example.com",
          "--no-oauth-exchange"
        ])

        config = Hex.Config.read()
        repo = config[:"$repos"]["reponame"]
        assert repo.oauth_exchange == false
      end)
    end

    test "add without --no-oauth-exchange enables OAuth token exchange by default" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame",
          "http://example.com"
        ])

        config = Hex.Config.read()
        repo = config[:"$repos"]["reponame"]
        assert repo.oauth_exchange == true
      end)
    end

    test "add with --oauth-exchange-url sets custom OAuth exchange URL" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame",
          "http://example.com",
          "--oauth-exchange-url",
          "http://custom-oauth.com"
        ])

        config = Hex.Config.read()
        repo = config[:"$repos"]["reponame"]
        assert repo.oauth_exchange_url == "http://custom-oauth.com"
      end)
    end

    test "add with both --no-oauth-exchange and --oauth-exchange-url" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame",
          "http://example.com",
          "--no-oauth-exchange",
          "--oauth-exchange-url",
          "http://custom-oauth.com"
        ])

        config = Hex.Config.read()
        repo = config[:"$repos"]["reponame"]
        assert repo.oauth_exchange == false
        assert repo.oauth_exchange_url == "http://custom-oauth.com"
      end)
    end

    test "set with --no-oauth-exchange disables OAuth token exchange" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run(["add", "reponame", "http://example.com"])
        Mix.Tasks.Hex.Repo.run(["set", "reponame", "--no-oauth-exchange"])

        config = Hex.Config.read()
        repo = config[:"$repos"]["reponame"]
        assert repo.oauth_exchange == false
      end)
    end

    test "set with --oauth-exchange-url updates custom OAuth exchange URL" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run(["add", "reponame", "http://example.com"])

        Mix.Tasks.Hex.Repo.run([
          "set",
          "reponame",
          "--oauth-exchange-url",
          "http://new-oauth.com"
        ])

        config = Hex.Config.read()
        repo = config[:"$repos"]["reponame"]
        assert repo.oauth_exchange_url == "http://new-oauth.com"
      end)
    end

    test "show with --oauth-exchange displays oauth_exchange setting" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame",
          "http://example.com",
          "--no-oauth-exchange"
        ])

        Mix.Tasks.Hex.Repo.run(["show", "reponame", "--oauth-exchange"])

        assert_received {:mix_shell, :info, ["false"]}
      end)
    end

    test "show with --oauth-exchange-url displays custom OAuth URL" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame",
          "http://example.com",
          "--oauth-exchange-url",
          "http://custom-oauth.com"
        ])

        Mix.Tasks.Hex.Repo.run(["show", "reponame", "--oauth-exchange-url"])

        assert_received {:mix_shell, :info, ["http://custom-oauth.com"]}
      end)
    end

    test "add with auth key and --no-oauth-exchange" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "reponame",
          "http://example.com",
          "--auth-key",
          "my-api-key",
          "--no-oauth-exchange"
        ])

        config = Hex.Config.read()
        repo = config[:"$repos"]["reponame"]
        assert repo.auth_key == "my-api-key"
        assert repo.oauth_exchange == false
      end)
    end

    test "list displays repos with OAuth exchange disabled" do
      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())

        Mix.Tasks.Hex.Repo.run([
          "add",
          "repo1",
          "http://example1.com",
          "--no-oauth-exchange"
        ])

        Mix.Tasks.Hex.Repo.run(["add", "repo2", "http://example2.com"])

        Mix.Tasks.Hex.Repo.run(["list"])

        assert_received {:mix_shell, :info, [header]}
        assert header =~ "Name"

        messages =
          Stream.unfold(nil, fn _ ->
            receive do
              {:mix_shell, :info, [msg]} -> {msg, nil}
            after
              0 -> nil
            end
          end)
          |> Enum.to_list()

        all_output = Enum.join([header | messages], "\n")
        assert all_output =~ "repo1"
        assert all_output =~ "repo2"
      end)
    end
  end
end
