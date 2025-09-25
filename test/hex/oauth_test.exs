defmodule Hex.OAuthTest do
  use HexTest.IntegrationCase

  describe "get_token/1" do
    test "returns error when no tokens are stored" do
      assert {:error, :no_auth} = Hex.OAuth.get_token(:read)
      assert {:error, :no_auth} = Hex.OAuth.get_token(:write)
    end

    test "returns valid token when available and not expired" do
      future_time = System.system_time(:second) + 3600

      tokens = %{
        "read" => %{
          "access_token" => "read_token",
          "refresh_token" => "read_refresh",
          "expires_at" => future_time
        },
        "write" => %{
          "access_token" => "write_token",
          "refresh_token" => "write_refresh",
          "expires_at" => future_time
        }
      }

      Hex.OAuth.store_tokens(tokens)

      assert {:ok, "read_token"} = Hex.OAuth.get_token(:read)
      assert {:ok, "write_token"} = Hex.OAuth.get_token(:write)
    end

    test "returns error when token is expired and no refresh possible" do
      past_time = System.system_time(:second) - 100

      tokens = %{
        "read" => %{
          "access_token" => "expired_token",
          "expires_at" => past_time
        }
      }

      Hex.OAuth.store_tokens(tokens)

      assert {:error, :token_expired} = Hex.OAuth.get_token(:read)
    end

    test "returns error when token is expired and refresh fails" do
      # Create expired token with invalid refresh token (since we can't test real refresh)
      past_time = System.system_time(:second) - 100

      expired_tokens = %{
        "write" => %{
          "access_token" => "expired_token",
          "refresh_token" => "invalid_refresh_token",
          "expires_at" => past_time
        }
      }

      Hex.OAuth.store_tokens(expired_tokens)

      # Should fail to refresh and return error
      assert {:error, :refresh_failed} = Hex.OAuth.get_token(:write)
    end
  end

  describe "store_tokens/1" do
    test "stores tokens in both config and state" do
      tokens = %{
        "read" => %{
          "access_token" => "read_token",
          "refresh_token" => "read_refresh",
          "expires_at" => System.system_time(:second) + 3600
        }
      }

      Hex.OAuth.store_tokens(tokens)

      # Check state
      assert Hex.State.get(:oauth_tokens) == tokens

      # Check config
      config = Hex.Config.read()
      assert config[:"$oauth_tokens"] == tokens
    end

    test "handles empty tokens" do
      Hex.OAuth.store_tokens(%{})

      assert Hex.State.get(:oauth_tokens) == %{}
      config = Hex.Config.read()
      assert config[:"$oauth_tokens"] == %{}
    end
  end

  describe "clear_tokens/0" do
    test "removes tokens from both config and state" do
      tokens = %{
        "read" => %{
          "access_token" => "token",
          "refresh_token" => "refresh",
          "expires_at" => System.system_time(:second) + 3600
        }
      }

      Hex.OAuth.store_tokens(tokens)
      assert Hex.OAuth.has_tokens?()

      Hex.OAuth.clear_tokens()

      assert Hex.State.get(:oauth_tokens) == nil
      refute Hex.OAuth.has_tokens?()
    end

    test "clears tokens from config file" do
      tokens = %{
        "write" => %{
          "access_token" => "config_token",
          "refresh_token" => "config_refresh",
          "expires_at" => System.system_time(:second) + 3600
        }
      }

      Hex.OAuth.store_tokens(tokens)

      # Verify token is in config
      config = Hex.Config.read()
      assert config[:"$oauth_tokens"]["write"]["access_token"] == "config_token"

      Hex.OAuth.clear_tokens()

      # Verify token is removed from config
      config = Hex.Config.read()
      assert is_list(config) or not Map.has_key?(config, :"$oauth_tokens")
    end
  end

  describe "has_tokens?/0" do
    test "returns false when no tokens are stored" do
      refute Hex.OAuth.has_tokens?()
    end

    test "returns true when tokens are stored" do
      tokens = %{
        "read" => %{
          "access_token" => "token",
          "refresh_token" => "refresh",
          "expires_at" => System.system_time(:second) + 3600
        }
      }

      Hex.OAuth.store_tokens(tokens)
      assert Hex.OAuth.has_tokens?()
    end

    test "returns true even with expired tokens" do
      past_time = System.system_time(:second) - 100

      tokens = %{
        "read" => %{
          "access_token" => "expired_token",
          "expires_at" => past_time
        }
      }

      Hex.OAuth.store_tokens(tokens)
      assert Hex.OAuth.has_tokens?()
    end
  end

  describe "create_token_data/1" do
    test "creates token data with proper expiration time" do
      current_time = System.system_time(:second)

      oauth_response = %{
        "access_token" => "test_token",
        "refresh_token" => "test_refresh",
        "expires_in" => 3600,
        "token_type" => "bearer",
        "scope" => "api"
      }

      token_data = Hex.OAuth.create_token_data(oauth_response)

      assert token_data["access_token"] == "test_token"
      assert token_data["refresh_token"] == "test_refresh"
      assert token_data["expires_at"] >= current_time + 3600
      # Allow 5 second margin
      assert token_data["expires_at"] <= current_time + 3600 + 5

      # Should only contain the three required fields
      assert Map.keys(token_data) |> Enum.sort() == [
               "access_token",
               "expires_at",
               "refresh_token"
             ]
    end

    test "handles missing refresh token" do
      oauth_response = %{
        "access_token" => "test_token",
        "expires_in" => 3600,
        "token_type" => "bearer",
        "scope" => "api"
      }

      token_data = Hex.OAuth.create_token_data(oauth_response)

      assert token_data["access_token"] == "test_token"
      refute Map.has_key?(token_data, "refresh_token")
      assert is_integer(token_data["expires_at"])
    end
  end

  test "token validation considers 60 second buffer" do
    # Token that expires in 30 seconds should still be returned (no refresh attempted without refresh_token)
    soon_expiry = System.system_time(:second) + 30

    tokens = %{
      "read" => %{
        "access_token" => "soon_expired_token",
        "expires_at" => soon_expiry
      }
    }

    Hex.OAuth.store_tokens(tokens)

    # Should return the token since no refresh is attempted without refresh_token
    assert {:ok, "soon_expired_token"} = Hex.OAuth.get_token(:read)
  end

  describe "refresh_token/1" do
    test "handles refresh token failure with real server" do
      # Test refresh token failure (since we can't create valid OAuth tokens)
      tokens = %{
        "write" => %{
          "access_token" => "old_token",
          "refresh_token" => "invalid_refresh_token",
          "expires_at" => System.system_time(:second) + 100
        }
      }

      Hex.OAuth.store_tokens(tokens)

      # This should make a real HTTP request to the server and fail
      assert {:error, :refresh_failed} = Hex.OAuth.refresh_token(:write)
    end

    test "handles refresh failure with invalid token" do
      tokens = %{
        "read" => %{
          "access_token" => "old_token",
          "refresh_token" => "definitely_invalid_refresh_token",
          "expires_at" => System.system_time(:second) + 100
        }
      }

      Hex.OAuth.store_tokens(tokens)

      # This should make a real HTTP request and fail
      assert {:error, :refresh_failed} = Hex.OAuth.refresh_token(:read)
    end

    test "returns error when no refresh token available" do
      tokens = %{
        "write" => %{
          "access_token" => "token_without_refresh",
          "expires_at" => System.system_time(:second) + 100
        }
      }

      Hex.OAuth.store_tokens(tokens)

      assert {:error, :no_refresh_token} = Hex.OAuth.refresh_token(:write)
    end

    test "returns error when no tokens stored" do
      assert {:error, :no_auth} = Hex.OAuth.refresh_token(:read)
    end

    test "handles network errors gracefully" do
      tokens = %{
        "write" => %{
          "access_token" => "token",
          "refresh_token" => "some_token",
          "expires_at" => System.system_time(:second) + 100
        }
      }

      Hex.OAuth.store_tokens(tokens)

      # Temporarily point to invalid URL to simulate network error
      original_url = Hex.State.fetch!(:api_url)
      Hex.State.put(:api_url, "http://invalid-host:9999/api")

      assert {:error, :refresh_failed} = Hex.OAuth.refresh_token(:write)

      # Restore original URL
      Hex.State.put(:api_url, original_url)
    end
  end
end
