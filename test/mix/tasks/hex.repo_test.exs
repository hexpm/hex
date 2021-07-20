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
        "AAAA"
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
end
