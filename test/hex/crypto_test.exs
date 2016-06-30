defmodule Hex.CryptoTest do
  use ExUnit.Case, async: true
  import Hex.Crypto

  @salt "saltsaltsaltsalt"

  test "encrypt and decrypt" do
    cipher = encrypt("password", @salt, "plain")
    assert  decrypt("password", @salt, cipher) == {:ok, "plain"}
  end

  test "encrypt and decrypt with tag" do
    cipher = encrypt("password", @salt, "plain", "tag")
    assert  decrypt("password", @salt, cipher, "tag") == {:ok, "plain"}
  end

  test "invalid password" do
    cipher = encrypt("passw0rd", @salt, "plain", "tag")
    assert  decrypt("password", @salt, cipher, "tag") == :error
  end
end
