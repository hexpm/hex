defmodule Mix.Tasks.Hex.PublishTrustedPublisherTest do
  use HexTest.Case

  @package "trusted_package"

  defmodule TrustedPackage.MixProject do
    def project do
      [
        app: :trusted_package,
        description: "trusted",
        version: "0.1.0",
        aliases: [docs: [&docs/1]],
        package: [
          licenses: ["MIT"],
          files: ["myfile.txt"],
          links: %{"a" => "http://a"}
        ]
      ]
    end

    defp docs(_) do
      File.mkdir_p!("doc")
      File.write!("doc/index.html", "the index")
    end
  end

  setup do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    System.put_env(
      "ACTIONS_ID_TOKEN_REQUEST_URL",
      "http://localhost:#{bypass.port}/github/token?api-version=2.0"
    )

    System.put_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "request_token")

    on_exit(fn ->
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_URL")
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
    end)

    Mix.Project.push(TrustedPackage.MixProject)
    {:ok, bypass: bypass}
  end

  @tag :requires_json
  test "publishes package and docs with one trusted publisher token", %{bypass: bypass} do
    stub_api(bypass)

    publish(["--yes", "--no-progress"])

    assert_received {:request, "GET", "/github/token", github_query, github_auth}
    assert URI.decode_query(github_query) == %{"api-version" => "2.0", "audience" => "hexpm"}
    assert github_auth == ["Bearer request_token"]

    assert_received {:exchange, exchange}

    assert exchange == %{
             "grant_type" => "urn:ietf:params:oauth:grant-type:jwt-bearer",
             "assertion" => "oidc_token",
             "scope" => "package:hexpm/#{@package}"
           }

    assert_received {:request, "POST", "/api/packages/#{@package}/releases", _, release_auth}
    assert release_auth == ["Bearer minted_token"]

    assert_received {:request, "POST", "/api/packages/#{@package}/releases/0.1.0/docs", _,
                     docs_auth}

    assert docs_auth == ["Bearer minted_token"]

    refute_received {:exchange, _}
    refute_received {:request, _method, "/api/users/me", _, _}
  end

  @tag :requires_json
  test "scopes the token to the organization repository", %{bypass: bypass} do
    stub_api(bypass)

    publish(["package", "--yes", "--no-progress", "--organization", "acme"])

    assert_received {:exchange, %{"scope" => "package:acme/#{@package}"}}

    assert_received {:request, "POST", "/api/repos/acme/packages/#{@package}/releases", _,
                     ["Bearer minted_token"]}
  end

  @tag :requires_json
  test "publishes docs alone with a trusted publisher token", %{bypass: bypass} do
    stub_api(bypass)

    publish(["docs", "--no-progress"])

    assert_received {:exchange, %{"scope" => "package:hexpm/#{@package}"}}

    assert_received {:request, "POST", "/api/packages/#{@package}/releases/0.1.0/docs", _,
                     ["Bearer minted_token"]}
  end

  test "an explicit API key takes precedence", %{bypass: bypass} do
    stub_api(bypass)
    Hex.State.put(:api_key, "api_key")

    publish(["package", "--yes", "--no-progress"])

    refute_received {:exchange, _}
    refute_received {:request, "GET", "/github/token", _, _}

    assert_received {:request, "POST", "/api/packages/#{@package}/releases", _, ["api_key"]}
  end

  test "a dry run does not exchange the OIDC token" do
    publish(["--yes", "--dry-run"])

    assert_received {:mix_shell, :info, ["Building #{@package} 0.1.0"]}
  end

  @tag :requires_json
  test "raises with the reason Hex refused the exchange", %{bypass: bypass} do
    stub_api(bypass,
      exchange:
        {403,
         %{"error" => "access_denied", "error_description" => "No matching trusted publisher"}}
    )

    message =
      "Trusted publishing failed, Hex did not grant a token for package:hexpm/#{@package}: " <>
        "access_denied: No matching trusted publisher (HTTP 403)"

    assert_raise Mix.Error, message, fn ->
      publish(["package", "--yes", "--no-progress"])
    end

    refute_received {:request, "POST", "/api/packages/" <> _, _, _}
  end

  defp publish(args) do
    in_tmp(fn ->
      File.write!("myfile.txt", "hello")
      Mix.Tasks.Hex.Publish.run(args)
    end)
  end

  defp stub_api(bypass, opts \\ []) do
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
           "scope" => "package:hexpm/#{@package}"
         }}
      )

    Bypass.expect(bypass, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn, length: 100_000_000)
      auth = Plug.Conn.get_req_header(conn, "authorization")
      send(test_pid, {:request, conn.method, conn.request_path, conn.query_string, auth})

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

        {"POST", _path} ->
          erlang_resp(conn, 201, %{"html_url" => "http://hex/#{@package}"})
      end
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end
end
