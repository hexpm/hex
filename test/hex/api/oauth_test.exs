defmodule Hex.API.OAuthTest do
  use HexTest.IntegrationCase

  setup do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    {:ok, bypass: bypass}
  end

  describe "device_authorization/1" do
    test "returns device authorization data", %{bypass: bypass} do
      expected_response = %{
        "device_code" => "test_device_code",
        "user_code" => "TEST123",
        "verification_uri" => "https://hex.pm/oauth/device",
        "verification_uri_complete" => "https://hex.pm/oauth/device?user_code=TEST123",
        "expires_in" => 600,
        "interval" => 5
      }

      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(expected_response))
      end)

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.device_authorization("api repositories")

      assert response == expected_response
    end

    test "defaults to api repositories scope", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(%{}))
      end)

      Hex.API.OAuth.device_authorization()
    end

    test "handles error response", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "invalid_request"}))
      end)

      assert {:ok, {400, _headers, %{"error" => "invalid_request"}}} =
               Hex.API.OAuth.device_authorization()
    end
  end

  describe "poll_device_token/1" do
    test "returns token on successful authorization", %{bypass: bypass} do
      expected_response = %{
        "access_token" => "access_token_value",
        "token_type" => "bearer",
        "expires_in" => 3600,
        "refresh_token" => "refresh_token_value",
        "scope" => "api repositories"
      }

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(expected_response))
      end)

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.poll_device_token("test_device_code")

      assert response == expected_response
    end

    test "returns authorization_pending when user hasn't authorized yet", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(
          400,
          Hex.Utils.safe_serialize_erlang(%{"error" => "authorization_pending"})
        )
      end)

      assert {:ok, {400, _headers, %{"error" => "authorization_pending"}}} =
               Hex.API.OAuth.poll_device_token("test_device_code")
    end

    test "returns slow_down when polling too frequently", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "slow_down"}))
      end)

      assert {:ok, {400, _headers, %{"error" => "slow_down"}}} =
               Hex.API.OAuth.poll_device_token("test_device_code")
    end
  end

  describe "exchange_token/2" do
    test "exchanges token for more limited scope", %{bypass: bypass} do
      expected_response = %{
        "access_token" => "limited_access_token",
        "token_type" => "bearer",
        "expires_in" => 3600,
        "refresh_token" => "limited_refresh_token",
        "scope" => "api:write"
      }

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(expected_response))
      end)

      assert {:ok, {200, _headers, response}} =
               Hex.API.OAuth.exchange_token("original_token", "api:write")

      assert response == expected_response
    end

    test "handles invalid token", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "invalid_grant"}))
      end)

      assert {:ok, {400, _headers, %{"error" => "invalid_grant"}}} =
               Hex.API.OAuth.exchange_token("invalid_token", "api:write")
    end
  end

  describe "refresh_token/1" do
    test "refreshes access token", %{bypass: bypass} do
      expected_response = %{
        "access_token" => "new_access_token",
        "token_type" => "bearer",
        "expires_in" => 3600,
        "refresh_token" => "new_refresh_token",
        "scope" => "api:write"
      }

      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(expected_response))
      end)

      assert {:ok, {200, _headers, response}} = Hex.API.OAuth.refresh_token("refresh_token_value")
      assert response == expected_response
    end

    test "handles invalid refresh token", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "invalid_grant"}))
      end)

      assert {:ok, {400, _headers, %{"error" => "invalid_grant"}}} =
               Hex.API.OAuth.refresh_token("invalid_refresh_token")
    end
  end

  describe "revoke_token/1" do
    test "revokes token successfully", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/revoke", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(""))
      end)

      assert {:ok, {200, _headers, ""}} = Hex.API.OAuth.revoke_token("token_to_revoke")
    end

    test "handles invalid token", %{bypass: bypass} do
      Bypass.expect(bypass, "POST", "/api/oauth/revoke", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "invalid_request"}))
      end)

      assert {:ok, {400, _headers, %{"error" => "invalid_request"}}} =
               Hex.API.OAuth.revoke_token("invalid_token")
    end
  end
end
