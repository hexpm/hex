defmodule Hex.API.OAuthTest do
  use HexTest.IntegrationCase, async: true

  # Using real test server at localhost:4043 with OAuth client configured

  describe "device_authorization/1" do
    test "returns device authorization data" do
      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.device_authorization("api repositories")

      # Verify the response has the expected structure from the real server
      assert is_binary(response["device_code"])
      assert is_binary(response["user_code"])
      assert is_binary(response["verification_uri"])
      assert is_integer(response["expires_in"])
      assert is_integer(response["interval"])
    end

    test "defaults to api repositories scope" do
      assert {:ok, {200, _headers, response}} = Hex.API.OAuth.device_authorization("api")

      # Should return valid device authorization data
      assert is_binary(response["device_code"])
      assert is_binary(response["user_code"])
    end

    test "handles invalid scope" do
      # The real server should handle invalid scopes - may accept or reject
      assert {:ok, {status, _headers, _response}} =
               Hex.API.OAuth.device_authorization("invalid_scope")

      # Server may return 200 (accepted), 400 (invalid scope), or 401 (invalid client)
      assert status in [200, 400, 401]
    end

    test "sends name parameter when provided" do
      name = "TestMachine"

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.device_authorization("api repositories", name)

      # Verify the response has the expected structure
      assert is_binary(response["device_code"])
      assert is_binary(response["user_code"])
    end

    test "works without name parameter" do
      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.device_authorization("api repositories", nil)

      # Should still return valid device authorization data
      assert is_binary(response["device_code"])
      assert is_binary(response["user_code"])
    end
  end

  describe "poll_device_token/1" do
    test "returns authorization_pending for valid device code" do
      # First get a valid device code
      {:ok, {200, _headers, device_response}} = Hex.API.OAuth.device_authorization("api")
      device_code = device_response["device_code"]

      # Polling should return authorization_pending since user hasn't authorized
      assert {:ok, {400, _headers, %{"error" => "authorization_pending"}}} =
               Hex.API.OAuth.poll_device_token(device_code)
    end

    test "returns invalid_grant for invalid device code" do
      assert {:ok, {400, _headers, %{"error" => "invalid_grant"}}} =
               Hex.API.OAuth.poll_device_token("invalid_device_code")
    end

    test "handles malformed device code" do
      assert {:ok, {400, _headers, %{"error" => error}}} =
               Hex.API.OAuth.poll_device_token("")

      assert error in ["invalid_grant", "invalid_request"]
    end
  end

  describe "refresh_token/1" do
    test "handles invalid refresh token" do
      # Test with a completely invalid refresh token
      assert {:ok, {status, _headers, %{"error" => error}}} =
               Hex.API.OAuth.refresh_token("invalid_refresh_token")

      assert status in [400, 401]
      assert error in ["invalid_token", "invalid_grant"]
    end

    test "handles malformed refresh token" do
      # Test with malformed refresh token
      assert {:ok, {status, _headers, %{"error" => error}}} =
               Hex.API.OAuth.refresh_token("malformed_token")

      assert status in [400, 401]
      assert error in ["invalid_token", "invalid_grant"]
    end

    test "handles empty refresh token" do
      assert {:ok, {400, _headers, %{"error" => error}}} =
               Hex.API.OAuth.refresh_token("")

      assert error in ["invalid_grant", "invalid_request"]
    end
  end

  describe "revoke_token/1" do
    test "returns 200 for token revocation" do
      # OAuth revoke endpoint returns 200 even for invalid tokens (per RFC 7009)
      assert {:ok, {200, _headers, _body}} = Hex.API.OAuth.revoke_token("any_token")
    end

    test "handles empty token" do
      assert {:ok, {200, _headers, _body}} = Hex.API.OAuth.revoke_token("")
    end
  end

  describe "exchange_api_key/3" do
    test "exchanges valid API key for OAuth access token" do
      auth = HexTest.Hexpm.new_user("apikey_user", "apikey@example.com", "password", "api_key")
      api_key = auth[:key]

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.exchange_api_key(api_key, "api")

      assert is_binary(response["access_token"])
      assert response["token_type"] == "bearer"
      assert is_integer(response["expires_in"])
      assert response["expires_in"] > 0
      assert response["scope"] == "api"
      refute Map.has_key?(response, "refresh_token")
    end

    test "exchanges API key with multiple scopes" do
      {:ok, {201, _, _}} =
        Hex.API.User.new("apikey_multi", "apikey_multi@example.com", "password")

      permissions = [%{"domain" => "api"}, %{"domain" => "repositories"}]

      {:ok, {201, _, %{"secret" => api_key}}} =
        Hex.API.Key.new("api_key_multi", permissions, user: "apikey_multi", pass: "password")

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.exchange_api_key(api_key, "api repositories")

      assert is_binary(response["access_token"])
      assert response["token_type"] == "bearer"
      assert response["scope"] == "api repository:hexpm"
    end

    test "accepts scopes as list" do
      {:ok, {201, _, _}} = Hex.API.User.new("apikey_list", "apikey_list@example.com", "password")

      permissions = [%{"domain" => "api"}, %{"domain" => "repositories"}]

      {:ok, {201, _, %{"secret" => api_key}}} =
        Hex.API.Key.new("api_key_list", permissions, user: "apikey_list", pass: "password")

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.exchange_api_key(api_key, ["api", "repositories"])

      assert is_binary(response["access_token"])
      assert response["scope"] == "api repository:hexpm"
    end

    test "sends name parameter when provided" do
      auth =
        HexTest.Hexpm.new_user(
          "apikey_named",
          "apikey_named@example.com",
          "password",
          "api_key_named"
        )

      api_key = auth[:key]

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.exchange_api_key(api_key, "api", "TestMachine")

      assert is_binary(response["access_token"])
    end

    test "works without name parameter" do
      auth =
        HexTest.Hexpm.new_user(
          "apikey_noname",
          "apikey_noname@example.com",
          "password",
          "api_key_noname"
        )

      api_key = auth[:key]

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.exchange_api_key(api_key, "api", nil)

      assert is_binary(response["access_token"])
    end

    test "returns error for invalid API key" do
      assert {:ok, {401, _headers, response}} =
               Hex.API.OAuth.exchange_api_key("invalid_api_key", "api")

      assert is_map(response)
      assert Map.has_key?(response, "message") or Map.has_key?(response, "error")
    end

    test "returns error for empty API key" do
      assert {:ok, {400, _headers, _response}} =
               Hex.API.OAuth.exchange_api_key("", "api")
    end

    test "handles malformed API key" do
      assert {:ok, {401, _headers, _response}} =
               Hex.API.OAuth.exchange_api_key("malformed-key", "api")
    end
  end
end
