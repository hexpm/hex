defmodule Hex.AuthTest do
  use HexTest.Case

  defp get_auth_config(repo) do
    Hex.Auth.callbacks().get_auth_config.(repo)
  end

  defp put_repo(name, config) do
    Hex.State.update!(:repos, &Map.put(&1, name, config))
  end

  describe "get_auth_config/1 callback" do
    test "returns the repo config when no HEX_API_KEY is set" do
      put_repo("acme", %{url: "https://acme.example", auth_key: "repo_key"})
      Hex.State.put(:api_key, nil)

      config = get_auth_config("acme")

      assert config.auth_key == "repo_key"
      refute Map.has_key?(config, :api_key)
    end

    test "surfaces HEX_API_KEY alongside the repo's own credentials" do
      # API and repo credentials authenticate different endpoints, so the env
      # api_key must not displace the repo's auth_key/oauth_token.
      put_repo("acme", %{url: "https://acme.example", auth_key: "repo_key"})
      Hex.State.put(:api_key, "env_api_key")

      config = get_auth_config("acme")

      assert config.api_key == "env_api_key"
      assert config.auth_key == "repo_key"
    end

    test "does not override a per-repo api_key with HEX_API_KEY" do
      put_repo("acme", %{url: "https://acme.example", api_key: "repo_api_key"})
      Hex.State.put(:api_key, "env_api_key")

      assert get_auth_config("acme").api_key == "repo_api_key"
    end

    test "returns just the api_key when the repo is unknown" do
      Hex.State.put(:api_key, "env_api_key")

      assert get_auth_config("does-not-exist") == %{api_key: "env_api_key"}
    end

    test "returns :undefined when the repo is unknown and no HEX_API_KEY is set" do
      Hex.State.put(:api_key, nil)

      assert get_auth_config("does-not-exist") == :undefined
    end
  end

  describe "clear_oauth_tokens/0 callback" do
    test "drops the in-memory token and warns to re-authenticate" do
      Hex.State.put(:oauth_token, %{
        access_token: "expired",
        refresh_token: "refresh",
        expires_at: System.system_time(:second) - 100
      })

      assert Hex.Auth.callbacks().clear_oauth_tokens.() == :ok

      assert Hex.State.get(:oauth_token) == nil

      output = Case.shell_output()
      assert output =~ "could not be refreshed"
      assert output =~ "mix hex.user auth"
    end

    test "keeps the on-disk token so a later run can retry the refresh" do
      in_tmp("clear_oauth_tokens", fn ->
        set_home_cwd()

        Hex.OAuth.store_token(%{
          access_token: "expired",
          refresh_token: "refresh",
          expires_at: System.system_time(:second) - 100
        })

        assert Hex.Auth.callbacks().clear_oauth_tokens.() == :ok

        assert Hex.State.get(:oauth_token) == nil
        assert Hex.Config.read()[:"$oauth_token"] != nil
      end)
    end
  end

  describe "persist_oauth_tokens/4 callback" do
    test "keeps the tokens repositories exchanged at the same time" do
      in_tmp("persist_repo_tokens", fn ->
        set_home_cwd()
        Hex.Config.write([])

        expires_at = System.system_time(:second) + 3600
        names = Enum.map(1..15, &"hexpm:org#{&1}")

        names
        |> Enum.map(fn name ->
          Task.async(fn ->
            Hex.Auth.callbacks().persist_oauth_tokens.(
              name,
              "token-#{name}",
              "refresh",
              expires_at
            )
          end)
        end)
        |> Task.await_many(10_000)

        repos = Hex.Config.read()[:"$repos"]

        for name <- names do
          assert repos[name].oauth_token.access_token == "token-#{name}"
        end
      end)
    end
  end

  describe "prompt_otp/1 callback" do
    test "returns the code that was typed" do
      send(self(), {:mix_shell_input, :prompt, "123456\n"})

      assert Hex.Auth.callbacks().prompt_otp.("OTP code:") == {:ok, "123456"}
      assert Hex.State.get(:api_otp) == "123456"
    end

    test "cancels when there is nothing to read" do
      send(self(), {:mix_shell_input, :prompt, :eof})

      assert Hex.Auth.callbacks().prompt_otp.("OTP code:") == :cancelled
    end
  end

  describe "SSO re-authentication" do
    test "stores the organizations the server flagged with the token" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()

        assert Hex.Auth.callbacks().sso_reauth.(["acme"]) == :ok

        assert Hex.OAuth.sso_reauth_required() == ["acme"]
        assert Hex.Config.read()[:"$oauth_token"][:sso_reauth_required] == ["acme"]
      end)
    end

    test "drops them again once nothing is flagged" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()

        assert Hex.Auth.callbacks().sso_reauth.(["acme"]) == :ok
        assert Hex.Auth.callbacks().sso_reauth.([]) == :ok

        assert Hex.OAuth.sso_reauth_required() == []
        refute Map.has_key?(Hex.State.get(:oauth_token), :sso_reauth_required)
      end)
    end

    test "keeps them when a refreshed token is persisted" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])

        expires_at = System.system_time(:second) + 3600

        assert Hex.Auth.callbacks().persist_oauth_tokens.(
                 :global,
                 "refreshed",
                 "refresh",
                 expires_at
               ) == :ok

        assert Hex.State.get(:oauth_token).access_token == "refreshed"
        assert Hex.OAuth.sso_reauth_required() == ["acme"]
        assert Hex.Config.read()[:"$oauth_token"][:sso_reauth_required] == ["acme"]
      end)
    end

    test "asks only about the organizations this resolution needs" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme", "widgets"])

        send(self(), {:mix_shell_input, :yes?, false})
        check_sso_reauth([{"hexpm:acme", "foo"}, {"hexpm", "ecto"}])

        assert_received {:mix_shell, :yes?, [question]}
        assert question =~ "acme requires SSO authentication"
        refute question =~ "widgets"
      end)
    end

    test "says what declining costs" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])

        send(self(), {:mix_shell_input, :yes?, false})
        check_sso_reauth([{"hexpm:acme", "foo"}])

        assert_received {:mix_shell, :yes?, _question}
        assert Case.shell_output() =~ "Packages from acme will not be available"
      end)
    end

    test "asks nothing about an organization authenticated with its own key" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])
        repos = Hex.State.fetch!(:repos)
        hexpm = repos["hexpm"]

        Hex.State.put(
          :repos,
          Map.put(repos, "hexpm:acme", %{hexpm | auth_key: "org-key"})
        )

        assert check_sso_reauth([{"hexpm:acme", "foo"}]) == :ok
        assert Case.shell_output() == ""
      end)
    end

    test "says so rather than asking when Hex is offline" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])
        Hex.State.put(:offline, true)

        check_sso_reauth([{"hexpm:acme", "foo"}])

        refute_received {:mix_shell, :yes?, _question}
        assert Case.shell_output() =~ "Hex is offline"
      end)
    end

    test "asks nothing when the project needs none of the flagged organizations" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])

        assert check_sso_reauth([{"hexpm", "ecto"}]) == :ok
        assert Case.shell_output() == ""
      end)
    end

    test "authenticates and picks the scopes up on the forced refresh" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])
        stub_sso_authorization(%{})

        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, ""})

        assert check_sso_reauth([{"hexpm:acme", "foo"}]) == :ok

        assert_received {:hex_system_cmd, _cmd, args}
        assert "https://hex.pm/sso/authorize/acme" in args

        assert Hex.OAuth.sso_reauth_required() == []
        assert Hex.State.get(:oauth_token).access_token == "renewed"
        assert Case.shell_output() == ""
      end)
    end

    test "asks for the URL as the stored session rather than as HEX_API_KEY" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])
        Hex.State.put(:api_key, "env_api_key")
        stub_sso_authorization(%{})

        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, ""})

        assert check_sso_reauth([{"hexpm:acme", "foo"}]) == :ok

        assert_received {:sso_authorization_header, "Bearer token"}
      end)
    end

    test "says what is unavailable when the session is still lapsed afterwards" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])
        stub_sso_authorization(%{"sso_reauth_required" => ["acme"]})

        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, ""})

        check_sso_reauth([{"hexpm:acme", "foo"}])

        assert Hex.OAuth.sso_reauth_required() == ["acme"]
        assert Case.shell_output() =~ "Packages from acme will not be available"
      end)
    end

    test "prints the authorization URL without the characters a terminal acts on" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])
        stub_sso_authorization(%{}, "https://hex.pm/sso/\e]0;pwned\a\nauthorize")

        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, ""})

        check_sso_reauth([{"hexpm:acme", "foo"}])

        assert_received {:mix_shell, :prompt, [prompt]}

        assert prompt ==
                 "Open https://hex.pm/sso/]0;pwnedauthorize to authenticate, then press enter"

        assert_received {:hex_system_cmd, _cmd, args}
        assert "https://hex.pm/sso/]0;pwnedauthorize" in args
      end)
    end

    test "says so when the authorization URL cannot be requested" do
      in_tmp("sso_reauth", fn ->
        set_home_cwd()
        store_token()
        Hex.Auth.callbacks().sso_reauth.(["acme"])

        bypass = Bypass.open()
        Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

        Bypass.expect(bypass, fn conn ->
          erlang_resp(conn, 422, %{"message" => "acme does not use SSO"})
        end)

        send(self(), {:mix_shell_input, :yes?, true})

        check_sso_reauth([{"hexpm:acme", "foo"}])

        refute_received {:hex_system_cmd, _cmd, _args}
        assert Case.shell_output() =~ "acme does not use SSO"
      end)
    end
  end

  describe "authentication preflight" do
    test "asks to authenticate when a private organization is needed and nothing is stored" do
      in_tmp("preflight", fn ->
        set_home_cwd()

        send(self(), {:mix_shell_input, :yes?, false})
        Hex.RemoteConverger.check_and_refresh_auth(["acme"])

        assert_received {:mix_shell, :yes?, [question]}
        assert question =~ "No authenticated user found"
        assert Case.shell_output() =~ "Private packages will not be available"
      end)
    end

    test "asks nothing when no organization needs the session" do
      in_tmp("preflight", fn ->
        set_home_cwd()

        assert Hex.RemoteConverger.check_and_refresh_auth([]) == :ok

        refute_received {:mix_shell, :yes?, _question}
        assert Case.shell_output() == ""
      end)
    end

    test "warns about the network, and keeps the session, when the renewal did not get through" do
      in_tmp("preflight", fn ->
        set_home_cwd()

        Hex.OAuth.store_token(%{
          access_token: "expired",
          refresh_token: "refresh",
          expires_at: System.system_time(:second) - 100
        })

        bypass = Bypass.open()
        Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
        Bypass.down(bypass)

        Hex.RemoteConverger.check_and_refresh_auth(["acme"])

        assert Case.shell_output() =~ "Could not reach Hex to renew your authentication"
        assert Hex.State.get(:oauth_token).access_token == "expired"
        assert Hex.Config.read()[:"$oauth_token"][:access_token] == "expired"
      end)
    end

    test "renews the session HEX_API_KEY does not stand in for" do
      in_tmp("preflight", fn ->
        set_home_cwd()

        Hex.OAuth.store_token(%{
          access_token: "expired",
          refresh_token: "refresh",
          expires_at: System.system_time(:second) - 100
        })

        Hex.State.put(:api_key, "env_api_key")
        stub_token_refresh(%{"sso_reauth_required" => ["acme"]})

        assert Hex.RemoteConverger.check_and_refresh_auth(["acme"]) == :ok

        assert Hex.State.get(:oauth_token).access_token == "renewed"
        assert Hex.OAuth.sso_reauth_required() == ["acme"]
      end)
    end

    test "does not renew the session when Hex is offline" do
      in_tmp("preflight", fn ->
        set_home_cwd()

        Hex.OAuth.store_token(%{
          access_token: "expired",
          refresh_token: "refresh",
          expires_at: System.system_time(:second) - 100
        })

        Hex.State.put(:offline, true)

        assert Hex.RemoteConverger.check_and_refresh_auth(["acme"]) == :ok

        refute_received {:mix_shell, :yes?, _question}
        assert Case.shell_output() == ""
        assert Hex.State.get(:oauth_token).access_token == "expired"
      end)
    end
  end

  defp check_sso_reauth(prefetches) do
    prefetches
    |> Hex.RemoteConverger.user_oauth_organizations()
    |> Hex.RemoteConverger.check_sso_reauth()
  end

  # Answers the two requests re-authorization makes: the URL the user opens, and
  # the refresh that picks up what completing it granted. Overrides go into the
  # refresh response, which is what says whether anything is still lapsed.
  defp stub_sso_authorization(refresh_overrides, verification_uri \\ nil) do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    test_pid = self()

    Bypass.expect(bypass, fn conn ->
      case conn.request_path do
        "/api/oauth/sso_authorization" ->
          [header] = Plug.Conn.get_req_header(conn, "authorization")
          send(test_pid, {:sso_authorization_header, header})

          {:ok, body, conn} = Plug.Conn.read_body(conn)
          %{"organizations" => organizations} = :erlang.binary_to_term(body)

          uri =
            verification_uri ||
              "https://hex.pm/sso/authorize/#{Enum.join(organizations, "-")}"

          erlang_resp(conn, 201, %{
            "verification_uri" => uri,
            "expires_in" => 600
          })

        "/api/oauth/token" ->
          payload = %{
            "access_token" => "renewed",
            "refresh_token" => "refresh",
            "token_type" => "Bearer",
            "expires_in" => 3600
          }

          erlang_resp(conn, 200, Map.merge(payload, refresh_overrides))
      end
    end)
  end

  defp stub_token_refresh(overrides) do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    Bypass.expect(bypass, fn conn ->
      assert conn.request_path == "/api/oauth/token"

      payload = %{
        "access_token" => "renewed",
        "refresh_token" => "refresh",
        "token_type" => "Bearer",
        "expires_in" => 3600
      }

      erlang_resp(conn, 200, Map.merge(payload, overrides))
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end

  defp store_token do
    Hex.OAuth.store_token(%{
      access_token: "token",
      refresh_token: "refresh",
      expires_at: System.system_time(:second) + 3600
    })
  end
end
