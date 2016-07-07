defmodule Hex.CryptoTest do
  use ExUnit.Case, async: true
  import Hex.Crypto

  test "encrypt and decrypt" do
    cipher = encrypt("plain", "password")
    assert  decrypt(cipher, "password") == {:ok, "plain"}
  end

  test "encrypt and decrypt with tag" do
    cipher = encrypt("plain", "password", "tag")
    assert  decrypt(cipher, "password", "tag") == {:ok, "plain"}
  end

  test "invalid password" do
    cipher = encrypt("plain", "passw0rd", "tag")
    assert  decrypt(cipher, "password", "tag") == :error
  end
end
