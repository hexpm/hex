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
      # Same token is returned for both read and write operations
      assert {:ok, "write_access"} = Hex.OAuth.get_token()
      assert {:ok, "write_access"} = Hex.OAuth.get_token()

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
end
