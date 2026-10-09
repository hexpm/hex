defmodule Hex.WorkloadIdentityTest do
  use HexTest.Case

  setup do
    System.put_env("ACTIONS_ID_TOKEN_REQUEST_URL", "http://localhost/token")
    System.put_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "request_token")

    on_exit(fn ->
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_URL")
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
    end)
  end

  describe "auth!/2 defers to configured credentials" do
    test "when the user authenticated" do
      in_tmp(fn ->
        set_home_cwd()

        Hex.OAuth.store_token(%{
          access_token: "token",
          expires_at: System.os_time(:second) + 3600
        })

        assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
      end)
    end

    test "when the hexpm repository has an API key" do
      Hex.State.update!(:repos, &put_in(&1["hexpm"][:api_key], "repo_api_key"))
      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
    end
  end

  describe "fetching from an organization's repository" do
    @refusal {403,
              %{
                "error" => "access_denied",
                "error_description" => "No matching workload identity"
              }}

    @refusal_message "Workload Identity authentication failed, Hex did not grant a token for " <>
                       "repository:acme: access_denied: No matching workload identity (HTTP 403). " <>
                       "To fetch without Workload Identity, authenticate with " <>
                       "`mix hex.organization auth acme --key KEY`"

    setup do
      bypass = Bypass.open()
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
      Hex.State.update!(:repos, &put_in(&1["hexpm"].url, "http://localhost:#{bypass.port}/repo"))

      System.put_env(
        "ACTIONS_ID_TOKEN_REQUEST_URL",
        "http://localhost:#{bypass.port}/github/token?api-version=2.0"
      )

      {:ok, bypass: bypass}
    end

    @tag :requires_json
    test "exchanges once per organization ahead of the fetches", %{bypass: bypass} do
      in_tmp(fn ->
        set_home_cwd()
        stub_repository(bypass)

        assert Hex.RemoteConverger.check_and_refresh_auth(["acme"]) == :ok

        assert_received {:exchange, exchange}

        assert exchange == %{
                 "grant_type" => "urn:ietf:params:oauth:grant-type:jwt-bearer",
                 "assertion" => "oidc_token",
                 "scope" => "repository:acme"
               }

        assert_received {:mix_shell, :info,
                         ["Authenticated to the acme organization with Workload Identity"]}

        refute_received {:mix_shell, :yes?, _question}

        fetch_tarballs()
        refute_received {:exchange, _}
      end)
    end

    @tag :requires_json
    test "exchanges once when fetching without the preflight", %{bypass: bypass} do
      in_tmp(fn ->
        set_home_cwd()
        stub_repository(bypass)

        fetch_tarballs()

        assert_received {:exchange, %{"scope" => "repository:acme"}}
        refute_received {:exchange, _}
        refute Map.has_key?(Hex.Config.read_repos(Hex.Config.read()), "hexpm:acme")
      end)
    end

    @tag :requires_json
    test "a fetch the repository refuses reports the refused exchange", %{bypass: bypass} do
      stub_repository(bypass, exchange: @refusal, repository: {401, ""})

      assert Hex.RemoteConverger.check_and_refresh_auth(["acme"]) == :ok
      refute_received {:mix_shell, :info, _message}

      assert {:error, {:auth_error, {:workload_identity_failed, reason}}} =
               Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")

      assert Hex.WorkloadIdentity.repository_error_message("hexpm:acme", reason) ==
               @refusal_message

      assert_received {:repository, "tarballs/foo-1.0.0.tar", []}
      assert_received {:exchange, _exchange}
      refute_received {:exchange, _exchange}
    end

    @tag :requires_json
    test "a refused exchange leaves the request to .netrc credentials", %{bypass: bypass} do
      on_exit(fn -> System.delete_env("NETRC") end)

      in_tmp(fn ->
        File.write!(".netrc", """
        machine localhost
          login mirror-user
          password mirror-password
        """)

        System.put_env("NETRC", Path.join(File.cwd!(), ".netrc"))
        stub_repository(bypass, exchange: @refusal)

        assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")

        basic = "Basic " <> Base.encode64("mirror-user:mirror-password")
        assert_received {:repository, "tarballs/foo-1.0.0.tar", [^basic]}
      end)
    end

    @tag :requires_json
    test "parallel registry fetches share one refused exchange", %{bypass: bypass} do
      stub_repository(bypass, exchange: @refusal, repository: {401, ""})
      Hex.State.put(:shell_process, self())

      Hex.Registry.Server.open(
        check_version: false,
        registry_path: tmp_path("workload_identity.ets")
      )

      packages = ["foo", "bar", "baz"]

      Hex.Registry.Server.prefetch(Enum.map(packages, &{"hexpm:acme", &1}))

      for package <- packages do
        assert Hex.Registry.Server.versions("hexpm:acme", package) == :error

        message = "Failed to fetch record for hexpm:acme/#{package} from registry"
        assert_received {:mix_shell, :error, [^message]}
        assert_received {:mix_shell, :error, [@refusal_message]}
      end

      assert_received {:exchange, _exchange}
      refute_received {:exchange, _exchange}
    end

    test "a stored user session takes precedence", %{bypass: bypass} do
      in_tmp(fn ->
        set_home_cwd()
        stub_repository(bypass)

        Hex.OAuth.store_token(%{
          access_token: "user_token",
          expires_at: System.os_time(:second) + 3600
        })

        assert Hex.RemoteConverger.check_and_refresh_auth(["acme"]) == :ok
        assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")

        assert_received {:repository, "tarballs/foo-1.0.0.tar", ["Bearer user_token"]}
        refute_received {:exchange, _}
      end)
    end
  end

  defp fetch_tarballs do
    assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
    assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "bar", "1.0.0")

    assert_received {:repository, "tarballs/foo-1.0.0.tar", ["Bearer minted_token"]}
    assert_received {:repository, "tarballs/bar-1.0.0.tar", ["Bearer minted_token"]}
  end

  defp stub_repository(bypass, opts \\ []) do
    test_pid = self()

    exchange =
      Keyword.get(
        opts,
        :exchange,
        {200,
         %{
           "access_token" => "minted_token",
           "token_type" => "bearer",
           "expires_in" => 900,
           "scope" => "repository:acme"
         }}
      )

    {repository_status, repository_body} = Keyword.get(opts, :repository, {200, "tarball"})

    Bypass.expect(bypass, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      auth = Plug.Conn.get_req_header(conn, "authorization")

      case {conn.method, conn.request_path} do
        {"GET", "/api/oidc/audience"} ->
          erlang_resp(conn, 200, %{"audience" => "hexpm"})

        {"GET", "/github/token"} ->
          conn
          |> Plug.Conn.put_resp_content_type("application/json")
          |> Plug.Conn.resp(200, ~s({"count":1,"value":"oidc_token"}))

        {"POST", "/api/oauth/token"} ->
          send(test_pid, {:exchange, :erlang.binary_to_term(body)})
          {status, payload} = exchange
          erlang_resp(conn, status, payload)

        {"GET", "/repo/repos/acme/" <> path} ->
          send(test_pid, {:repository, path, auth})
          Plug.Conn.resp(conn, repository_status, repository_body)
      end
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end
end
