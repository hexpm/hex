defmodule Hex.OAuthTest do
  use HexTest.IntegrationCase

  describe "get_token/0" do
    test "returns error when no tokens are stored" do
      assert {:error, :no_auth} = Hex.OAuth.get_token()
    end

    test "returns valid token when available and not expired" do
      future_time = System.system_time(:second) + 3600

      token_data = %{
        "access_token" => "test_token",
        "refresh_token" => "test_refresh",
        "expires_at" => future_time
      }

      Hex.OAuth.store_token(token_data)

      assert {:ok, "test_token"} = Hex.OAuth.get_token()
    end

    test "returns error when token is expired and no refresh possible" do
      past_time = System.system_time(:second) - 100

      token_data = %{
        "access_token" => "expired_token",
        "expires_at" => past_time
      }

      Hex.OAuth.store_token(token_data)

      assert {:error, :token_expired} = Hex.OAuth.get_token()
    end

    test "returns error when token is expired and refresh fails" do
      past_time = System.system_time(:second) - 100

      token_data = %{
        "access_token" => "expired_token",
        "refresh_token" => "invalid_refresh_token",
        "expires_at" => past_time
      }

      Hex.OAuth.store_token(token_data)

      # Should fail to refresh and return error
      assert {:error, :refresh_failed} = Hex.OAuth.get_token()
    end
  end

  describe "store_token/1" do
    test "stores token in both config and state" do
      token_data = %{
        "access_token" => "test_token",
        "refresh_token" => "test_refresh",
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.OAuth.store_token(token_data)

      # Check state
      assert Hex.State.get(:oauth_token) == token_data

      # Check config
      config = Hex.Config.read()
      assert config[:"$oauth_token"] == token_data
    end

    test "handles empty token" do
      Hex.OAuth.store_token(%{})

      assert Hex.State.get(:oauth_token) == %{}
      config = Hex.Config.read()
      assert config[:"$oauth_token"] == %{}
    end
  end

  describe "clear_tokens/0" do
    test "removes tokens from both config and state" do
      token_data = %{
        "access_token" => "token",
        "refresh_token" => "refresh",
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.OAuth.store_token(token_data)
      assert Hex.OAuth.has_tokens?()

      Hex.OAuth.clear_tokens()

      assert Hex.State.get(:oauth_token) == nil
      refute Hex.OAuth.has_tokens?()
    end

    test "clears tokens from config file" do
      token_data = %{
        "access_token" => "config_token",
        "refresh_token" => "config_refresh",
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.OAuth.store_token(token_data)

      # Verify token is in config
      config = Hex.Config.read()
      assert config[:"$oauth_token"]["access_token"] == "config_token"

      Hex.OAuth.clear_tokens()

      # Verify token is removed from config
      config = Hex.Config.read()
      refute config[:"$oauth_token"]
    end
  end

  describe "has_tokens?/0" do
    test "returns false when no tokens are stored" do
      refute Hex.OAuth.has_tokens?()
    end

    test "returns true when tokens are stored" do
      token_data = %{
        "access_token" => "token",
        "refresh_token" => "refresh",
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.OAuth.store_token(token_data)
      assert Hex.OAuth.has_tokens?()
    end

    test "returns true even with expired tokens" do
      past_time = System.system_time(:second) - 100

      token_data = %{
        "access_token" => "expired_token",
        "expires_at" => past_time
      }

      Hex.OAuth.store_token(token_data)
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

  describe "refresh_token/0" do
    test "returns error when no refresh token available" do
      token_data = %{
        "access_token" => "token_without_refresh",
        "expires_at" => System.system_time(:second) + 100
      }

      Hex.OAuth.store_token(token_data)

      assert {:error, :no_refresh_token} = Hex.OAuth.refresh_token()
    end

    test "returns error when no tokens stored" do
      assert {:error, :no_auth} = Hex.OAuth.refresh_token()
    end
  end
end
