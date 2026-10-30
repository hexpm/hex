defmodule Mix.Tasks.Hex.PublishWorkloadIdentityTest do
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
  test "publishes package and docs with one workload identity token", %{bypass: bypass} do
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

    assert_received {:mix_shell, :info, ["Authenticated with Workload Identity"]}
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
  test "publishes docs alone with a workload identity token", %{bypass: bypass} do
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
    refute_received {:mix_shell, :info, ["Authenticated with Workload Identity"]}

    assert_received {:request, "POST", "/api/packages/#{@package}/releases", _, ["api_key"]}
  end

  @tag :requires_json
  test "an empty HEX_API_KEY publishes with the workload identity", %{bypass: bypass} do
    stub_api(bypass)
    original = System.get_env("HEX_API_KEY")
    System.put_env("HEX_API_KEY", "")

    on_exit(fn ->
      if original,
        do: System.put_env("HEX_API_KEY", original),
        else: System.delete_env("HEX_API_KEY")
    end)

    Hex.State.put_all(Map.merge(Hex.State.get_all(), Map.take(Hex.State.init([]), [:api_key])))

    publish(["package", "--yes", "--no-progress"])

    assert_received {:exchange, %{"scope" => "package:hexpm/#{@package}"}}

    assert_received {:request, "POST", "/api/packages/#{@package}/releases", _,
                     ["Bearer minted_token"]}
  end

  test "a dry run does not exchange the OIDC token" do
    publish(["--yes", "--dry-run", "--wait"])

    assert_received {:mix_shell, :info, ["Building #{@package} 0.1.0"]}
  end

  @tag :requires_json
  test "raises with the reason Hex refused the exchange", %{bypass: bypass} do
    stub_api(bypass,
      exchange:
        {403,
         %{"error" => "access_denied", "error_description" => "No matching workload identity"}}
    )

    message =
      "Workload Identity authentication failed, Hex did not grant a token for package:hexpm/#{@package}: " <>
        "access_denied: No matching workload identity (HTTP 403)"

    assert_raise Mix.Error, message, fn ->
      publish(["package", "--yes", "--no-progress"])
    end

    refute_received {:request, "POST", "/api/packages/" <> _, _, _}
  end

  @tag :requires_json
  test "escapes the reason Hex refused the exchange", %{bypass: bypass} do
    stub_api(bypass,
      exchange:
        {403,
         %{
           "error" => "access_denied\e[2K\rFORGED",
           "error_description" => "No matching workload identity\e[2K\rFORGED"
         }}
    )

    error =
      assert_raise Mix.Error, fn ->
        publish(["package", "--yes", "--no-progress"])
      end

    assert error.message =~
             "access_denied\\e[2K\\rFORGED: No matching workload identity\\e[2K\\rFORGED (HTTP 403)"

    refute error.message =~ "\e"
    refute error.message =~ "\r"
  end

  test "escapes the message of a failed OIDC audience request", %{bypass: bypass} do
    stub_api(bypass, audience: {500, %{"message" => "Internal error\e[2K\rFORGED"}})

    error =
      assert_raise Mix.Error, fn ->
        publish(["package", "--yes", "--no-progress"])
      end

    assert error.message ==
             "Workload Identity authentication failed, could not fetch the OIDC audience " <>
               "from Hex: Internal error\\e[2K\\rFORGED (HTTP 500)"

    refute_received {:request, "GET", "/github/token", _, _}
  end

  test "prints staged package and docs with checksums of the uploaded bytes without polling", %{
    bypass: bypass
  } do
    Hex.State.put(:api_key, "api_key")
    stub_api(bypass, publish: staged())

    publish(["--yes", "--no-progress"])

    assert_received {:mix_shell, :info,
                     [
                       "Package staged for approval at https://hex.pm/dashboard/staged-releases/42 " <>
                         "(expires at 2026-11-01T12:00:00Z)"
                     ]}

    assert_received {:mix_shell, :info,
                     [
                       "Documentation staged for approval at https://hex.pm/dashboard/staged-releases/42 " <>
                         "(expires at 2026-11-01T12:00:00Z)"
                     ]}

    assert_received {:upload, package_path, package_bytes}
    refute String.ends_with?(package_path, "/docs")
    assert_received {:upload, "/api/packages/#{@package}/releases/0.1.0/docs", docs_bytes}
    package_checksum = Base.encode16(:crypto.hash(:sha256, package_bytes), case: :lower)
    docs_checksum = Base.encode16(:crypto.hash(:sha256, docs_bytes), case: :lower)
    assert_received {:mix_shell, :info, ["Package SHA-256: " <> ^package_checksum]}
    assert_received {:mix_shell, :info, ["Documentation SHA-256: " <> ^docs_checksum]}

    refute_received {:request, "GET", "/api/staged-releases/42", _, _}
    refute shell_output() =~ "published to"
  end

  test "waits after docs are uploaded and polls until approved", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")
    test_pid = self()
    {:ok, polls} = Agent.start_link(fn -> 0 end)

    with_docs = Map.put(staged(), "approval_checksum", "with-docs")

    stub_api(bypass,
      publish: staged(),
      docs: with_docs,
      poll: fn conn ->
        count = Agent.get_and_update(polls, &{&1, &1 + 1})
        send(test_pid, {:poll, count})

        erlang_resp(
          conn,
          200,
          Map.put(with_docs, "status", if(count == 0, do: "staged", else: "approved"))
        )
      end
    )

    publish(["--yes", "--no-progress", "--wait"])

    messages = flush()

    docs =
      Enum.find_index(
        messages,
        &match?({:request, "POST", "/api/packages/#{@package}/releases/0.1.0/docs", _, _}, &1)
      )

    poll = Enum.find_index(messages, &match?({:poll, 0}, &1))
    assert docs < poll
    assert {:poll, 1} in messages
    assert {:mix_shell, :info, ["Publish approved"]} in messages

    assert Enum.any?(
             messages,
             &match?({:hex_system_cmd, _, ["https://hex.pm/dashboard/staged-releases/42"]}, &1)
           )
  end

  test "refuses docs attached to another run's staged package", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")
    stub_api(bypass, publish: staged(), docs: Map.put(staged(), "checksum", "another-package"))

    assert_raise Mix.Error, "Staged package changed before its docs were staged", fn ->
      publish(["--yes", "--no-progress", "--wait"])
    end

    refute_received {:request, "GET", "/api/staged-releases/42", _, _}
  end

  test "verifies the original package when docs require a separate approval", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")
    docs = Map.merge(staged(), %{"id" => 43, "kind" => "docs", "approval_checksum" => "docs"})

    stub_api(bypass,
      publish: staged(),
      docs: docs,
      poll: fn conn ->
        payload =
          if conn.request_path == "/api/staged-releases/42",
            do: staged("approved"),
            else: Map.put(docs, "status", "approved")

        erlang_resp(conn, 200, payload)
      end
    )

    publish(["--yes", "--no-progress", "--wait"])

    requests =
      for {:request, "GET", path, _, _} <- flush(),
          String.starts_with?(path, "/api/staged-releases/"),
          do: path

    assert requests == ["/api/staged-releases/42", "/api/staged-releases/43"]
  end

  test "waits for docs-only approval in an organization repository", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")
    stub_api(bypass, publish: staged(), poll: &erlang_resp(&1, 200, staged("approved")))

    publish(["docs", "--no-progress", "--organization", "acme", "--wait"])

    assert_received {:request, "GET", "/api/repos/acme/staged-releases/42", _, ["api_key"]}
    assert_received {:mix_shell, :info, ["Publish approved"]}
  end

  for status <- ["rejected", "expired"] do
    test "fails when a staged publish is #{status}", %{bypass: bypass} do
      Hex.State.put(:api_key, "api_key")
      status = unquote(status)
      stub_api(bypass, publish: staged(), poll: &erlang_resp(&1, 200, staged(status)))

      assert_raise Mix.Error, "Staged publish #{status}", fn ->
        publish(["package", "--yes", "--no-progress", "--wait"])
      end
    end
  end

  test "fails if the staged contents change while waiting", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")
    changed = Map.put(staged("approved"), "approval_checksum", "different")
    stub_api(bypass, publish: staged(), poll: &erlang_resp(&1, 200, changed))

    assert_raise Mix.Error, "Staged publish changed while waiting for approval", fn ->
      publish(["package", "--yes", "--no-progress", "--wait"])
    end
  end

  @tag :requires_json
  test "renews an expired workload identity token while waiting", %{bypass: bypass} do
    {:ok, exchanges} = Agent.start_link(fn -> 0 end)

    exchange = fn conn ->
      token = Agent.get_and_update(exchanges, &{&1 + 1, &1 + 1})
      erlang_resp(conn, 200, %{"access_token" => "token#{token}", "expires_in" => 900})
    end

    poll = fn conn ->
      case Plug.Conn.get_req_header(conn, "authorization") do
        ["Bearer token1"] ->
          conn
          |> Plug.Conn.put_resp_header(
            "www-authenticate",
            ~s(Bearer realm="hex", error="invalid_token")
          )
          |> erlang_resp(401, %{"message" => "expired token"})

        ["Bearer token2"] ->
          erlang_resp(conn, 200, staged("approved"))
      end
    end

    stub_api(bypass, publish: staged(), exchange: exchange, poll: poll)
    publish(["package", "--yes", "--no-progress", "--wait"])

    assert Agent.get(exchanges, & &1) == 2
    assert_received {:request, "GET", "/api/staged-releases/42", _, ["Bearer token1"]}
    assert_received {:request, "GET", "/api/staged-releases/42", _, ["Bearer token2"]}
    assert_received {:mix_shell, :info, ["Publish approved"]}
  end

  test "doesn't wait when the docs upload fails", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")
    stub_api(bypass, publish: staged(), docs: %{"message" => "Docs rejected"}, docs_status: 422)
    assert catch_throw(publish(["--yes", "--no-progress", "--wait"])) == {:exit_code, 1}
    refute_received {:request, "GET", "/api/staged-releases/42", _, _}
  end

  test "reports a failed lookup without exchanging configured credentials", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")

    stub_api(bypass,
      publish: staged(),
      poll: &erlang_resp(&1, 401, %{"message" => "revoked key"})
    )

    assert_raise Mix.Error, "Failed to read staged publish status", fn ->
      publish(["package", "--yes", "--no-progress", "--wait"])
    end

    refute_received {:exchange, _}
  end

  test "doesn't poll a direct publish with --wait", %{bypass: bypass} do
    Hex.State.put(:api_key, "api_key")
    stub_api(bypass)
    publish(["package", "--yes", "--no-progress", "--wait"])
    refute_received {:request, "GET", "/api/staged-releases/" <> _, _, _}
    assert shell_output() =~ "Package published to"
  end

  defp staged(status \\ "staged") do
    %{
      "id" => 42,
      "kind" => "release",
      "checksum" => "package-checksum",
      "status" => status,
      "html_url" => "https://hex.pm/dashboard/staged-releases/42",
      "expires_at" => "2026-11-01T12:00:00Z",
      "approval_checksum" => "package-and-docs-checksum"
    }
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

    {audience_status, audience} = Keyword.get(opts, :audience, {200, %{"audience" => "hexpm"}})

    Bypass.expect(bypass, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn, length: 100_000_000)
      auth = Plug.Conn.get_req_header(conn, "authorization")
      send(test_pid, {:request, conn.method, conn.request_path, conn.query_string, auth})

      case {conn.method, conn.request_path} do
        {"GET", "/api/oidc/audience"} ->
          erlang_resp(conn, audience_status, audience)

        {"GET", "/github/token"} ->
          conn
          |> Plug.Conn.put_resp_content_type("application/json")
          |> Plug.Conn.resp(200, ~s({"count":1,"value":"oidc_token"}))

        {"POST", "/api/oauth/token"} ->
          send(test_pid, {:exchange, :erlang.binary_to_term(body)})

          if is_function(exchange) do
            exchange.(conn)
          else
            {status, payload} = exchange
            erlang_resp(conn, status, payload)
          end

        {"GET", path}
        when path in [
               "/api/staged-releases/42",
               "/api/staged-releases/43",
               "/api/repos/acme/staged-releases/42"
             ] ->
          Keyword.fetch!(opts, :poll).(conn)

        {"POST", path} ->
          send(test_pid, {:upload, path, body})
          payload = Keyword.get(opts, :publish, %{"html_url" => "http://hex/#{@package}"})

          payload =
            if String.ends_with?(path, "/docs"),
              do: Keyword.get(opts, :docs, payload),
              else: payload

          erlang_resp(
            conn,
            if(String.ends_with?(path, "/docs"),
              do: Keyword.get(opts, :docs_status, 201),
              else: 201
            ),
            payload
          )
      end
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end
end
