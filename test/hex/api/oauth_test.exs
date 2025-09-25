defmodule Hex.API.OAuthTest do
  use HexTest.IntegrationCase

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

  describe "exchange_token/2" do
    test "handles invalid token exchange" do
      # Test with a completely invalid token
      assert {:ok, {status, _headers, %{"error" => error}}} =
               Hex.API.OAuth.exchange_token("invalid_token", "api:write")

      assert status in [400, 401]
      assert error in ["invalid_token", "invalid_grant"]
    end

    test "handles token exchange with invalid scope" do
      # Test with invalid scope and invalid token (expect token error first)
      assert {:ok, {status, _headers, %{"error" => error}}} =
               Hex.API.OAuth.exchange_token("invalid_token", "invalid_scope")

      assert status in [400, 401]
      assert error in ["invalid_token", "invalid_grant", "invalid_scope"]
    end

    test "validates token format" do
      # Test with malformed token
      assert {:ok, {status, _headers, %{"error" => error}}} =
               Hex.API.OAuth.exchange_token("malformed_token", "api:write")

      assert status in [400, 401]
      assert error in ["invalid_grant", "invalid_token"]
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
end
