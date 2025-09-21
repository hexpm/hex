defmodule Mix.Tasks.Hex.UserTest do
  use HexTest.IntegrationCase

  test "auth performs OAuth device flow" do
    in_tmp(fn ->
      set_home_cwd()

      # Set up Bypass to mock OAuth endpoints
      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      # Clear any existing auth
      Hex.OAuth.clear_tokens()
      Hex.State.put(:api_key_write, nil)
      Hex.State.put(:api_key_read, nil)

      # Track which endpoints are called in order
      calls = Agent.start_link(fn -> [] end)

      # Mock device authorization
      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        Agent.update(elem(calls, 1), &(&1 ++ [:device_auth]))

        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(
          200,
          Hex.Utils.safe_serialize_erlang(%{
            "device_code" => "test_device_code",
            "user_code" => "TEST-CODE",
            "verification_uri" => "https://hex.pm/oauth/device",
            "expires_in" => 600,
            # No delay for testing
            "interval" => 0
          })
        )
      end)

      # Mock polling - succeed immediately for testing
      poll_count = Agent.start_link(fn -> 0 end)

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        {:ok, body, conn} = Plug.Conn.read_body(conn)

        # Check if this is device polling or token exchange
        cond do
          String.contains?(body, "device_code") ->
            # Device polling
            Agent.update(elem(poll_count, 1), &(&1 + 1))
            count = Agent.get(elem(poll_count, 1), & &1)
            Agent.update(elem(calls, 1), &(&1 ++ [:poll]))

            if count == 1 do
              # First poll - pending
              conn
              |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
              |> Plug.Conn.resp(
                400,
                Hex.Utils.safe_serialize_erlang(%{"error" => "authorization_pending"})
              )
            else
              # Second poll - success
              conn
              |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
              |> Plug.Conn.resp(
                200,
                Hex.Utils.safe_serialize_erlang(%{
                  "access_token" => "initial_token",
                  "token_type" => "bearer",
                  "expires_in" => 3600,
                  "refresh_token" => "initial_refresh",
                  "scope" => "api repositories"
                })
              )
            end

          String.contains?(body, "api:write") ->
            # Token exchange for write scope
            Agent.update(elem(calls, 1), &(&1 ++ [:exchange_write]))

            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(
              200,
              Hex.Utils.safe_serialize_erlang(%{
                "access_token" => "write_token",
                "token_type" => "bearer",
                "expires_in" => 3600,
                "refresh_token" => "write_refresh",
                "scope" => "api:write"
              })
            )

          String.contains?(body, "api:read") ->
            # Token exchange for read scope
            Agent.update(elem(calls, 1), &(&1 ++ [:exchange_read]))

            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(
              200,
              Hex.Utils.safe_serialize_erlang(%{
                "access_token" => "read_token",
                "token_type" => "bearer",
                "expires_in" => 3600,
                "refresh_token" => "read_refresh",
                "scope" => "api:read repositories"
              })
            )

          true ->
            conn
            |> Plug.Conn.resp(500, "Unexpected request")
        end
      end)

      # Run the auth task
      Mix.Tasks.Hex.User.run(["auth"])

      # Verify the device flow messages were shown using assert_received
      assert_received {:mix_shell, :info, ["Starting OAuth device flow authentication..."]}
      assert_received {:mix_shell, :info, ["To authenticate, visit: https://hex.pm/oauth/device"]}
      assert_received {:mix_shell, :info, ["And enter the code: TEST-CODE"]}
      assert_received {:mix_shell, :info, [""]}
      assert_received {:mix_shell, :info, ["Waiting for authentication..."]}
      assert_received {:mix_shell, :info, ["Authentication successful! Exchanging tokens..."]}
      assert_received {:mix_shell, :info, ["Authentication completed successfully!"]}

      # Verify the correct sequence of API calls
      call_sequence = Agent.get(elem(calls, 1), & &1)
      assert call_sequence == [:device_auth, :poll, :poll, :exchange_write, :exchange_read]

      # Verify tokens were stored correctly
      assert Hex.OAuth.has_tokens?()
      assert {:ok, "write_token"} = Hex.OAuth.get_token(:write)
      assert {:ok, "read_token"} = Hex.OAuth.get_token(:read)

      # Verify tokens are in config
      config = Hex.Config.read()
      assert config[:"$oauth_tokens"]["write"]["access_token"] == "write_token"
      assert config[:"$oauth_tokens"]["read"]["access_token"] == "read_token"

      # Cleanup
      Hex.State.put(:api_url, original_url)
    end)
  end

  test "auth handles device flow errors gracefully" do
    in_tmp(fn ->
      set_home_cwd()

      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      Hex.OAuth.clear_tokens()

      # Test expired device code
      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(
          200,
          Hex.Utils.safe_serialize_erlang(%{
            "device_code" => "expired_code",
            "user_code" => "EXPIRED",
            "verification_uri" => "https://hex.pm/oauth/device",
            "expires_in" => 600,
            "interval" => 0
          })
        )
      end)

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "expired_token"}))
      end)

      Mix.Tasks.Hex.User.run(["auth"])

      assert_received {:mix_shell, :error, ["Device code expired. Please try again."]}
      refute Hex.OAuth.has_tokens?()

      Hex.State.put(:api_url, original_url)
    end)
  end

  test "auth handles slow_down response" do
    in_tmp(fn ->
      set_home_cwd()

      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      Hex.OAuth.clear_tokens()

      # Mock device authorization
      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(
          200,
          Hex.Utils.safe_serialize_erlang(%{
            "device_code" => "slow_code",
            "user_code" => "SLOW",
            "verification_uri" => "https://hex.pm/oauth/device",
            "expires_in" => 600,
            "interval" => 0
          })
        )
      end)

      poll_count = Agent.start_link(fn -> 0 end)

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        {:ok, body, conn} = Plug.Conn.read_body(conn)
        Agent.update(elem(poll_count, 1), &(&1 + 1))
        count = Agent.get(elem(poll_count, 1), & &1)

        cond do
          count == 1 ->
            # First request - slow down
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "slow_down"}))

          String.contains?(body, "device_code") ->
            # Second device poll - success
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(
              200,
              Hex.Utils.safe_serialize_erlang(%{
                "access_token" => "token_after_slowdown",
                "token_type" => "bearer",
                "expires_in" => 3600,
                "refresh_token" => "refresh_after_slowdown",
                "scope" => "api repositories"
              })
            )

          true ->
            # Token exchange requests
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(
              200,
              Hex.Utils.safe_serialize_erlang(%{
                "access_token" => "exchanged",
                "refresh_token" => "exchanged_refresh",
                "expires_in" => 3600,
                "scope" =>
                  if(String.contains?(body, "api:write"),
                    do: "api:write",
                    else: "api:read repositories"
                  )
              })
            )
        end
      end)

      Mix.Tasks.Hex.User.run(["auth"])

      # Verify auth succeeded after slow_down
      assert Hex.OAuth.has_tokens?()

      Hex.State.put(:api_url, original_url)
    end)
  end

  test "auth handles user denial" do
    in_tmp(fn ->
      set_home_cwd()

      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      Hex.OAuth.clear_tokens()

      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(
          200,
          Hex.Utils.safe_serialize_erlang(%{
            "device_code" => "denied_code",
            "user_code" => "DENY",
            "verification_uri" => "https://hex.pm/oauth/device",
            "expires_in" => 600,
            "interval" => 0
          })
        )
      end)

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(403, Hex.Utils.safe_serialize_erlang(%{"error" => "access_denied"}))
      end)

      Mix.Tasks.Hex.User.run(["auth"])

      assert_received {:mix_shell, :error, ["Authentication was denied."]}
      refute Hex.OAuth.has_tokens?()

      Hex.State.put(:api_url, original_url)
    end)
  end

  test "inline authentication when no auth present" do
    in_tmp(fn ->
      set_home_cwd()

      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      # Clear all auth
      Hex.OAuth.clear_tokens()
      Hex.State.put(:api_key_write, nil)
      Hex.State.put(:api_key_read, nil)

      # User says yes to authenticate inline
      send(self(), {:mix_shell_input, :yes?, true})

      # Mock the OAuth flow for inline auth
      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(
          200,
          Hex.Utils.safe_serialize_erlang(%{
            "device_code" => "inline_device",
            "user_code" => "INLINE",
            "verification_uri" => "https://hex.pm/oauth/device",
            "expires_in" => 600,
            "interval" => 0
          })
        )
      end)

      # Expect multiple token requests: poll, exchange for write, exchange for read
      token_request_count = :counters.new(1, [])

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        {:ok, body, conn} = Plug.Conn.read_body(conn)
        params = Hex.Utils.safe_deserialize_erlang(body)

        count = :counters.get(token_request_count, 1)
        :counters.add(token_request_count, 1, 1)

        resp_body = case params["grant_type"] do
          "urn:ietf:params:oauth:grant-type:device_code" ->
            # First request is polling for device token
            %{
              "access_token" => "inline_token",
              "token_type" => "bearer",
              "expires_in" => 3600,
              "refresh_token" => "inline_refresh",
              "scope" => "api repositories"
            }
          "urn:ietf:params:oauth:grant-type:token-exchange" ->
            # Exchange requests based on requested scope
            case params["scope"] do
              "api:write" ->
                %{
                  "access_token" => "write_token",
                  "token_type" => "bearer",
                  "expires_in" => 3600,
                  "refresh_token" => "write_refresh",
                  "scope" => "api:write"
                }
              "api:read repositories" ->
                %{
                  "access_token" => "read_token",
                  "token_type" => "bearer",
                  "expires_in" => 3600,
                  "refresh_token" => "read_refresh",
                  "scope" => "api:read repositories"
                }
            end
        end

        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(resp_body))
      end)

      # Calling auth_info should trigger inline auth
      auth = Mix.Tasks.Hex.auth_info(:write)

      # Should get auth after inline flow with OAuth flag
      assert [key: _token, oauth: true] = auth

      assert_received {:mix_shell, :yes?,
                       ["No authenticated user found. Do you want to authenticate now?"]}

      Hex.State.put(:api_url, original_url)
    end)
  end

  test "inline authentication declined by user" do
    in_tmp(fn ->
      set_home_cwd()

      # Clear all auth
      Hex.OAuth.clear_tokens()
      Hex.State.put(:api_key_write, nil)
      Hex.State.put(:api_key_read, nil)

      # User says no to authenticate inline
      send(self(), {:mix_shell_input, :yes?, false})

      # Should raise when user declines
      assert_raise Mix.Error, "No authenticated user found. Run `mix hex.user auth`", fn ->
        Mix.Tasks.Hex.auth_info(:write)
      end

      assert_received {:mix_shell, :yes?,
                       ["No authenticated user found. Do you want to authenticate now?"]}
    end)
  end

  test "auth_info fallback behavior" do
    in_tmp(fn ->
      set_home_cwd()

      # Test fallback from OAuth to API keys
      Hex.OAuth.clear_tokens()

      # No auth should trigger inline auth (but we disable it)
      assert [] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      # Test with API key set
      Hex.State.put(:api_key_write, "test_api_key")
      assert [key: "test_api_key"] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      # Test with OAuth tokens
      future_time = System.system_time(:second) + 3600

      tokens = %{
        "write" => %{
          "access_token" => "oauth_token",
          "refresh_token" => "oauth_refresh",
          "expires_at" => future_time
        }
      }

      Hex.OAuth.store_tokens(tokens)
      assert [key: "oauth_token", oauth: true] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      # Clear OAuth tokens - should fall back to API key
      Hex.OAuth.clear_tokens()
      assert [key: "test_api_key"] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)
    end)
  end

  test "deauth user and organizations" do
    in_tmp(fn ->
      set_home_cwd()

      # Create OAuth tokens instead of password-based auth
      auth = Hexpm.new_oauth_user("userdeauth1", "userdeauth1@mail.com", "password")
      Hexpm.new_repo("myorguserdeauth1", auth)

      # Verify OAuth tokens exist
      assert Hex.Config.read()[:"$oauth_tokens"]

      # Create organization auth (this will use old method for now)
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorguserdeauth1"])

      Mix.Tasks.Hex.User.run(["deauth"])

      # Verify OAuth tokens are cleared
      refute Hex.Config.read()[:"$oauth_tokens"]
      refute Hex.Config.read()[:"$repos"]["hexpm:myorguserdeauth1"]
    end)
  end

  test "auth handles token exchange failure" do
    in_tmp(fn ->
      set_home_cwd()

      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      Hex.OAuth.clear_tokens()

      # Mock successful device flow but failed token exchange
      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(
          200,
          Hex.Utils.safe_serialize_erlang(%{
            "device_code" => "exchange_fail_code",
            "user_code" => "FAIL",
            "verification_uri" => "https://hex.pm/oauth/device",
            "expires_in" => 600,
            "interval" => 0
          })
        )
      end)

      call_count = Agent.start_link(fn -> 0 end)

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        {:ok, body, conn} = Plug.Conn.read_body(conn)
        Agent.update(elem(call_count, 1), &(&1 + 1))
        count = Agent.get(elem(call_count, 1), & &1)

        cond do
          count == 1 ->
            # Device token success
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(
              200,
              Hex.Utils.safe_serialize_erlang(%{
                "access_token" => "initial_token",
                "token_type" => "bearer",
                "expires_in" => 3600,
                "refresh_token" => "initial_refresh",
                "scope" => "api repositories"
              })
            )

          String.contains?(body, "api:write") ->
            # Token exchange for write fails
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "invalid_grant"}))

          true ->
            conn
            |> Plug.Conn.resp(500, "Unexpected request")
        end
      end)

      Mix.Tasks.Hex.User.run(["auth"])

      # Should show error for failed exchange
      assert_received {:mix_shell, :error, ["Failed to exchange write token" <> _]}

      # Should not have tokens stored due to exchange failure
      refute Hex.OAuth.has_tokens?()

      Hex.State.put(:api_url, original_url)
    end)
  end

  test "whoami" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("whoami", "whoami@mail.com", "password", "key")
      Mix.Tasks.Hex.update_keys(auth[:key], auth[:key])

      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, ["whoami"]}
    end)
  end
end
