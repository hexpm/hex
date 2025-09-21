defmodule Mix.Tasks.HexOAuthIntegrationTest do
  use HexTest.IntegrationCase

  describe "OAuth device flow integration" do
    setup do
      # Clear any existing tokens
      Hex.OAuth.clear_tokens()

      on_exit(fn ->
        Hex.OAuth.clear_tokens()
      end)

      :ok
    end

    test "token storage and retrieval with expiration" do
      # Test that tokens are properly stored with the right structure
      oauth_write_response = %{
        "access_token" => "write_access_token",
        "refresh_token" => "write_refresh_token",
        "expires_in" => 3600,
        "token_type" => "bearer",
        "scope" => "api:write"
      }

      oauth_read_response = %{
        "access_token" => "read_access_token",
        "refresh_token" => "read_refresh_token",
        "expires_in" => 7200,
        "token_type" => "bearer",
        "scope" => "api:read repositories"
      }

      # Test token data creation
      write_token_data = Hex.OAuth.create_token_data(oauth_write_response)
      read_token_data = Hex.OAuth.create_token_data(oauth_read_response)

      assert write_token_data["access_token"] == "write_access_token"
      assert write_token_data["refresh_token"] == "write_refresh_token"
      assert read_token_data["access_token"] == "read_access_token"
      assert read_token_data["refresh_token"] == "read_refresh_token"

      # Store both tokens
      tokens = %{
        "write" => write_token_data,
        "read" => read_token_data
      }

      Hex.OAuth.store_tokens(tokens)

      # Verify retrieval works
      assert {:ok, "write_access_token"} = Hex.OAuth.get_token(:write)
      assert {:ok, "read_access_token"} = Hex.OAuth.get_token(:read)

      # Verify tokens exist
      assert Hex.OAuth.has_tokens?()

      # Clear and verify
      Hex.OAuth.clear_tokens()
      refute Hex.OAuth.has_tokens?()
      assert {:error, :no_auth} = Hex.OAuth.get_token(:write)
    end

    test "token expiration and validation" do
      # Test expired token detection
      past_time = System.system_time(:second) - 100
      future_time = System.system_time(:second) + 3600

      expired_tokens = %{
        "write" => %{
          "access_token" => "expired_token",
          "refresh_token" => "expired_refresh",
          "expires_at" => past_time
        }
      }

      valid_tokens = %{
        "read" => %{
          "access_token" => "valid_token",
          "refresh_token" => "valid_refresh",
          "expires_at" => future_time
        }
      }

      # Test expired token with refresh token returns refresh_failed (since no server is mocked)
      Hex.OAuth.store_tokens(expired_tokens)
      assert {:error, :refresh_failed} = Hex.OAuth.get_token(:write)

      # Test valid token works
      Hex.OAuth.store_tokens(valid_tokens)
      assert {:ok, "valid_token"} = Hex.OAuth.get_token(:read)

      # Test mixed tokens - one valid, one expired
      mixed_tokens = Map.merge(expired_tokens, valid_tokens)
      Hex.OAuth.store_tokens(mixed_tokens)

      assert {:error, :refresh_failed} = Hex.OAuth.get_token(:write)
      assert {:ok, "valid_token"} = Hex.OAuth.get_token(:read)

      # Test expired token without refresh token returns token_expired
      expired_no_refresh = %{
        "write" => %{
          "access_token" => "expired_token_no_refresh",
          "expires_at" => past_time
        }
      }
      Hex.OAuth.store_tokens(expired_no_refresh)
      assert {:error, :token_expired} = Hex.OAuth.get_token(:write)
    end

    # These HTTP-level tests are already covered in oauth_test.exs
    # Here we focus on integration logic
  end

  describe "auth_info integration with OAuth tokens" do
    setup do
      bypass = Bypass.open()
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      # Clear any existing tokens and API keys
      Hex.OAuth.clear_tokens()
      Hex.State.put(:api_key_write, nil)
      Hex.State.put(:api_key_read, nil)
      Hex.State.put(:api_key_write_unencrypted, nil)

      on_exit(fn ->
        Hex.State.put(:api_url, original_url)
        Hex.OAuth.clear_tokens()
      end)

      {:ok, bypass: bypass}
    end

    test "auth_info returns OAuth token when available" do
      future_time = System.system_time(:second) + 3600

      tokens = %{
        "write" => %{
          "access_token" => "oauth_write_token",
          "refresh_token" => "oauth_write_refresh",
          "expires_at" => future_time
        },
        "read" => %{
          "access_token" => "oauth_read_token",
          "refresh_token" => "oauth_read_refresh",
          "expires_at" => future_time
        }
      }

      Hex.OAuth.store_tokens(tokens)

      assert [key: "oauth_write_token", oauth: true] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)
      assert [key: "oauth_read_token", oauth: true] = Mix.Tasks.Hex.auth_info(:read, auth_inline: false)
    end

    test "auth_info falls back to API key when no OAuth tokens" do
      Hex.State.put(:api_key_write, "fallback_write_key")
      Hex.State.put(:api_key_read, "fallback_read_key")

      assert [key: "fallback_write_key"] = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)
      assert [key: "fallback_read_key"] = Mix.Tasks.Hex.auth_info(:read, auth_inline: false)
    end

    test "auth_info uses write key for read when no separate read key", %{bypass: _bypass} do
      Hex.State.put(:api_key_write, "write_only_key")
      Hex.State.put(:api_key_read, nil)

      assert [key: "write_only_key"] = Mix.Tasks.Hex.auth_info(:read, auth_inline: false)
    end

    test "auth_info handles token expiration message", %{bypass: bypass} do
      # Set up expired token
      past_time = System.system_time(:second) - 100

      tokens = %{
        "write" => %{
          "access_token" => "expired_token",
          "refresh_token" => "expired_refresh",
          "expires_at" => past_time
        }
      }

      Hex.OAuth.store_tokens(tokens)

      # Mock failed refresh
      Bypass.expect(bypass, "POST", "/api/oauth/token", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
        |> Plug.Conn.resp(400, Hex.Utils.safe_serialize_erlang(%{"error" => "invalid_grant"}))
      end)

      # Should detect expiration and show message
      result = Mix.Tasks.Hex.auth_info(:write, auth_inline: false)
      assert result == []

      # Verify the expired token message would be shown
      # (In actual usage, this would trigger authenticate_inline)
    end
  end
end
