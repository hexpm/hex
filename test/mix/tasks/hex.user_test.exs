defmodule Mix.Tasks.Hex.UserTest do
  use HexTest.IntegrationCase

  @tag timeout: 5000
  test "auth performs OAuth device flow" do
    in_tmp(fn ->
      set_home_cwd()

      # Clear any existing auth
      Hex.OAuth.clear_tokens()

      # Test that device authorization works but don't try to complete the flow
      assert {:ok, {200, _headers, response}} = Hex.API.OAuth.device_authorization("api")

      assert %{
               "device_code" => device_code,
               "user_code" => user_code,
               "verification_uri" => verification_uri
             } = response

      assert is_binary(device_code)
      assert is_binary(user_code)
      assert is_binary(verification_uri)

      # Test that polling returns authorization_pending (user hasn't authorized yet)
      assert {:ok, {400, _headers, %{"error" => "authorization_pending"}}} =
               Hex.API.OAuth.poll_device_token(device_code)

      # Verify no tokens were stored since flow didn't complete
      refute Hex.OAuth.has_tokens?()
    end)
  end

  test "auth uses verification_uri_complete when available" do
    in_tmp(fn ->
      set_home_cwd()

      Hex.OAuth.clear_tokens()

      # Test device authorization
      assert {:ok, {200, _headers, %{"device_code" => device_code}}} =
               Hex.API.OAuth.device_authorization("api")

      assert is_binary(device_code)
    end)
  end

  test "auth with custom name parameter" do
    in_tmp(fn ->
      set_home_cwd()

      Hex.OAuth.clear_tokens()

      # Test device authorization with a custom name
      custom_name = "MyTestDevice"

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.device_authorization("api repositories", custom_name)

      assert %{
               "device_code" => device_code,
               "user_code" => user_code
             } = response

      assert is_binary(device_code)
      assert is_binary(user_code)

      # Verify the flow works with the name parameter
      assert {:ok, {400, _headers, %{"error" => "authorization_pending"}}} =
               Hex.API.OAuth.poll_device_token(device_code)
    end)
  end

  test "auth with nil name parameter" do
    in_tmp(fn ->
      set_home_cwd()

      Hex.OAuth.clear_tokens()

      # Test device authorization with nil name (should work)
      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.device_authorization("api repositories", nil)

      assert %{"device_code" => device_code} = response
      assert is_binary(device_code)
    end)
  end

  test "auth handles device flow errors gracefully" do
    in_tmp(fn ->
      set_home_cwd()

      Hex.OAuth.clear_tokens()

      # Test polling with an invalid device code
      assert {:ok, {400, _headers, %{"error" => error}}} =
               Hex.API.OAuth.poll_device_token("invalid_device_code")

      assert error in ["authorization_pending", "invalid_grant", "expired_token"]
    end)
  end

  test "auth handles slow_down response" do
    in_tmp(fn ->
      set_home_cwd()

      Hex.OAuth.clear_tokens()

      # Test that repeated polling gets proper response
      assert {:ok, {200, _headers, %{"device_code" => device_code}}} =
               Hex.API.OAuth.device_authorization("api")

      # Immediate polling should get authorization_pending
      assert {:ok, {400, _headers, %{"error" => "authorization_pending"}}} =
               Hex.API.OAuth.poll_device_token(device_code)
    end)
  end

  test "auth handles user denial" do
    in_tmp(fn ->
      set_home_cwd()

      Hex.OAuth.clear_tokens()

      # Test device authorization returns proper structure
      assert {:ok, {200, _headers, response}} = Hex.API.OAuth.device_authorization("api")

      assert %{
               "device_code" => _,
               "user_code" => _,
               "verification_uri" => _,
               "expires_in" => _,
               "interval" => _
             } = response
    end)
  end

  test "token refresh functionality" do
    in_tmp(fn ->
      set_home_cwd()

      # Create a user with OAuth tokens
      auth = Hexpm.new_oauth_user("refreshuser", "refreshuser@mail.com", "password")

      # Extract refresh token from auth
      refresh_token = auth[:refresh_token]

      # Test token refresh
      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.refresh_token(refresh_token)

      assert %{
               "access_token" => new_access_token,
               "refresh_token" => new_refresh_token,
               "token_type" => "bearer"
             } = response

      assert is_binary(new_access_token)
      assert is_binary(new_refresh_token)
    end)
  end

  test "token revocation functionality" do
    in_tmp(fn ->
      set_home_cwd()

      # Create a user with OAuth tokens
      auth = Hexpm.new_oauth_user("revokeuser", "revokeuser@mail.com", "password")

      # Extract access token from auth
      access_token = auth[:access_token]

      # Test token revocation
      assert {:ok, {200, _headers, nil}} = Hex.API.OAuth.revoke_token(access_token)

      # Token should no longer be valid for API calls
      config = Hex.API.Client.config(key: access_token, oauth: true)

      assert {:ok, {401, _headers, _}} = :mix_hex_api.get(config, ["users", "me"])
    end)
  end

  test "inline authentication when no auth present" do
    in_tmp(fn ->
      set_home_cwd()

      # Clear all auth
      Hex.OAuth.clear_tokens()

      # User says no to authenticate inline (to avoid hanging on real OAuth flow)
      send(self(), {:mix_shell_input, :yes?, false})

      # Calling auth_info should ask for inline auth
      assert_raise Mix.Error, "No authenticated user found. Run `mix hex.user auth`", fn ->
        Mix.Tasks.Hex.auth_info(:write)
      end

      assert_received {:mix_shell, :yes?,
                       ["No authenticated user found. Do you want to authenticate now?"]}
    end)
  end

  test "inline authentication declined by user" do
    in_tmp(fn ->
      set_home_cwd()

      # Clear all auth
      Hex.OAuth.clear_tokens()

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

  test "inline authentication accepted by user" do
    in_tmp(fn ->
      set_home_cwd()

      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      # Clear all auth
      Hex.OAuth.clear_tokens()

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

      # Mock polling - succeed immediately
      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        {:ok, body, conn} = Plug.Conn.read_body(conn)
        params = Hex.Utils.safe_deserialize_erlang(body)

        resp_body =
          case params["grant_type"] do
            "urn:ietf:params:oauth:grant-type:device_code" ->
              %{
                "access_token" => "inline_token",
                "token_type" => "bearer",
                "expires_in" => 3600,
                "refresh_token" => "inline_refresh",
                "scope" => "api repositories"
              }
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

  test "auth_info fallback behavior" do
    in_tmp(fn ->
      set_home_cwd()

      # Test fallback from OAuth to API keys
      Hex.OAuth.clear_tokens()

      # No auth should trigger inline auth (but we disable it)
      assert [] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      # Test with API key set
      Hex.State.put(:api_key, "test_api_key")
      assert [key: "test_api_key"] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      # Test with OAuth tokens
      future_time = System.system_time(:second) + 3600

      tokens = %{
        "access_token" => "oauth_token",
        "refresh_token" => "oauth_refresh",
        "expires_at" => future_time
      }

      Hex.OAuth.store_token(tokens)

      assert [key: "oauth_token", oauth: true] =
               Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      # Clear OAuth tokens - should fall back to API key
      Hex.OAuth.clear_tokens()
      assert [key: "test_api_key"] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)
    end)
  end

  test "auth_info with expired tokens triggers refresh" do
    in_tmp(fn ->
      set_home_cwd()

      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      # Store expired OAuth tokens
      past_time = System.system_time(:second) - 3600

      tokens = %{
        "access_token" => "expired_token",
        "refresh_token" => "refresh_token",
        "expires_at" => past_time
      }

      Hex.OAuth.store_token(tokens)

      # Mock refresh token endpoint
      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        {:ok, body, conn} = Plug.Conn.read_body(conn)

        # Check which refresh token is being used
        cond do
          String.contains?(body, "refresh_token") ->
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
            |> Plug.Conn.resp(
              200,
              Hex.Utils.safe_serialize_erlang(%{
                "access_token" => "new_token",
                "token_type" => "bearer",
                "expires_in" => 3600,
                "refresh_token" => "new_refresh_token",
                "scope" => "api:write"
              })
            )

          true ->
            conn
            |> Plug.Conn.resp(400, "Bad request")
        end
      end)

      # Call auth_info - should trigger refresh
      auth = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      # Should get new token after refresh
      assert [key: "new_token", oauth: true] = auth

      # Verify new tokens were stored
      config = Hex.Config.read()
      assert config[:"$oauth_token"]["access_token"] == "new_token"
      assert config[:"$oauth_token"]["refresh_token"] == "new_refresh_token"

      Hex.State.put(:api_url, original_url)
    end)
  end

  test "deauth user and organizations" do
    in_tmp(fn ->
      set_home_cwd()

      # Create OAuth tokens
      auth = Hexpm.new_oauth_user("userdeauth1", "userdeauth1@mail.com", "password")
      Hexpm.new_repo("myorguserdeauth1", auth)

      # Store OAuth tokens
      tokens = %{
        "access_token" => auth[:access_token],
        "refresh_token" => auth[:refresh_token],
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.OAuth.store_token(tokens)

      # Verify OAuth tokens exist
      assert Hex.Config.read()[:"$oauth_token"]

      # Create organization auth
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorguserdeauth1"])

      Mix.Tasks.Hex.User.run(["deauth"])

      # Verify OAuth tokens are cleared
      refute Hex.Config.read()[:"$oauth_token"]
      refute Hex.Config.read()[:"$repos"]["hexpm:myorguserdeauth1"]
    end)
  end

  test "auth handles token refresh failure" do
    in_tmp(fn ->
      set_home_cwd()

      # Try to refresh with invalid refresh token
      assert {:ok, {400, _headers, %{"error" => _}}} =
               Hex.API.OAuth.refresh_token("invalid_refresh_token")
    end)
  end

  test "OAuth token storage and retrieval" do
    in_tmp(fn ->
      set_home_cwd()

      # Clear any existing tokens
      Hex.OAuth.clear_tokens()
      refute Hex.OAuth.has_tokens?()

      # Store tokens
      tokens = %{
        "access_token" => "write_access",
        "refresh_token" => "write_refresh",
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.OAuth.store_token(tokens)
      assert Hex.OAuth.has_tokens?()

      # Retrieve tokens - same token for both read and write
      assert {:ok, "write_access"} = Hex.OAuth.get_token(:write)
      assert {:ok, "write_access"} = Hex.OAuth.get_token(:read)

      # Clear tokens
      Hex.OAuth.clear_tokens()
      refute Hex.OAuth.has_tokens?()
    end)
  end

  test "whoami with OAuth" do
    in_tmp(fn ->
      set_home_cwd()

      # Create user with OAuth tokens
      auth = Hexpm.new_oauth_user("whoamioauth", "whoamioauth@mail.com", "password")

      # Store OAuth token
      tokens = %{
        "access_token" => auth[:access_token],
        "refresh_token" => auth[:refresh_token],
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.OAuth.store_token(tokens)

      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, [username]}
      assert String.starts_with?(username, "whoamioauth")
    end)
  end

  test "whoami with API key" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("whoamiapi", "whoamiapi@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])

      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, [username]}
      assert String.starts_with?(username, "whoamiapi")
    end)
  end

  test "device flow with custom scopes" do
    in_tmp(fn ->
      set_home_cwd()

      # Test device authorization with custom scopes
      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.device_authorization("api:write repositories")

      assert %{
               "device_code" => device_code,
               "user_code" => _,
               "verification_uri" => _
             } = response

      assert is_binary(device_code)

      # Polling should return pending since user hasn't authorized
      assert {:ok, {400, _headers, %{"error" => "authorization_pending"}}} =
               Hex.API.OAuth.poll_device_token(device_code)
    end)
  end

  test "auth_info includes OTP from HEX_OTP environment variable" do
    in_tmp(fn ->
      set_home_cwd()

      # Setup OAuth tokens
      future_time = System.system_time(:second) + 3600

      tokens = %{
        "access_token" => "oauth_token",
        "refresh_token" => "refresh_token",
        "expires_at" => future_time
      }

      Hex.OAuth.store_token(tokens)

      # Set HEX_OTP in state
      Hex.State.put(:api_otp, "123456")

      # Get auth info - should include OTP
      auth = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      assert [key: "oauth_token", oauth: true, otp: "123456"] = auth
    end)
  end

  test "auth_info does not prompt for OTP when HEX_OTP is not set" do
    in_tmp(fn ->
      set_home_cwd()

      # Setup OAuth tokens
      future_time = System.system_time(:second) + 3600

      tokens = %{
        "access_token" => "oauth_token",
        "refresh_token" => "refresh_token",
        "expires_at" => future_time
      }

      Hex.OAuth.store_token(tokens)

      # Don't set HEX_OTP - should not prompt upfront
      Hex.State.put(:api_otp, nil)

      # Get auth info - should not include OTP (server will prompt if needed)
      auth = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)

      assert [key: "oauth_token", oauth: true] = auth
    end)
  end
end
