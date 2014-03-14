defmodule Hex.APITest do
  use HexTest.Case
  @moduletag :integration

  test "user" do
    assert { 404, _ } = Hex.API.get_user("test_user")

    assert { 201, _ } = Hex.API.new_user("test_user", "test_user@mail.com", "hunter42")
    assert { 200, body } = Hex.API.get_user("test_user")
    assert body["username"] == "test_user"

    auth = [user: "test_user", pass: "hunter42"]
    assert { 200, _ } = Hex.API.update_user("new_mail@mail.com", nil, auth)
    assert { 200, body } = Hex.API.get_user("test_user")
    assert body["email"] == "new_mail@mail.com"
  end

  test "package" do
    auth = [user: "user", pass: "hunter42"]

    assert { 404, _ } = Hex.API.get_package("ecto")
    assert { 201, _ } = Hex.API.new_package("ecto", [description: "foobar"], auth)
    assert { 200, body } = Hex.API.get_package("ecto")
    assert body["meta"]["description"] == "foobar"
  end

  test "release" do
    auth = [user: "user", pass: "hunter42"]
    Hex.API.new_package("postgrex", [], auth)
    Hex.API.new_package("decimal", [], auth)

    tar = Hex.Tar.create([app: :postgrex, version: "0.0.1", requirements: []], [])
    assert { 404, _ } = Hex.API.get_release("postgrex", "0.0.1")
    assert { 201, _ } = Hex.API.new_release("postgrex", tar, auth)
    assert { 200, body } = Hex.API.get_release("postgrex", "0.0.1")
    assert body["requirements"] == []

    tar = Hex.Tar.create([app: :decimal, version: "0.0.2", requirements: [postgrex: "~> 0.0.1"]], [])
    reqs = [{ "postgrex", "~> 0.0.1" }]
    assert { 201, _ } = Hex.API.new_release("decimal", tar, auth)
    assert { 200, body } = Hex.API.get_release("decimal", "0.0.2")
    assert body["requirements"] == reqs

    assert { 204, _ } = Hex.API.delete_release("decimal", "0.0.2", auth)
    assert { 404, _ } = Hex.API.get_release("decimal", "0.0.2")
  end

  test "registry" do
    HexWeb.RegistryBuilder.rebuild
    HexWeb.RegistryBuilder.wait_for_build

    assert { 200, _ } = Hex.API.get_registry
  end

  test "x-hex-message" do
    Hex.API.handle_hex_message('"oops, you done goofed"')
    refute_received { :mix_shell, _, _ }

    Hex.API.handle_hex_message('  "oops, you done goofed" ; level = warn')
    assert_received { :mix_shell, :info, ["API warning: oops, you done goofed"] }

    Hex.API.handle_hex_message('"oops, you done goofed";level=fatal  ')
    assert_received { :mix_shell, :error, ["API error: oops, you done goofed"] }
  end
end
