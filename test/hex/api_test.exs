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

    assert { 404, _ } = Hex.API.get_package("apple")
    assert { 201, _ } = Hex.API.new_package("apple", [description: "foobar"], auth)
    assert { 200, body } = Hex.API.get_package("apple")
    assert body["meta"]["description"] == "foobar"
  end

  test "release" do
    auth = [user: "user", pass: "hunter42"]
    Hex.API.new_package("pear", [], auth)
    Hex.API.new_package("grape", [], auth)

    tar = Hex.Tar.create([app: :pear, version: "0.0.1", requirements: []], [])
    assert { 404, _ } = Hex.API.get_release("pear", "0.0.1")
    assert { 201, _ } = Hex.API.new_release("pear", tar, auth)
    assert { 200, body } = Hex.API.get_release("pear", "0.0.1")
    assert body["requirements"] == %{}

    tar = Hex.Tar.create([app: :grape, version: "0.0.2", requirements: [pear: "~> 0.0.1"]], [])
    reqs = %{"pear" => %{"requirement" => "~> 0.0.1", "optional" => false}}
    assert { 201, _ } = Hex.API.new_release("grape", tar, auth)
    assert { 200, body } = Hex.API.get_release("grape", "0.0.2")
    assert body["requirements"] == reqs

    assert { 204, _ } = Hex.API.delete_release("grape", "0.0.2", auth)
    assert { 404, _ } = Hex.API.get_release("grape", "0.0.2")
  end

  test "registry" do
    HexWeb.RegistryBuilder.sync_rebuild
    assert { 200, _ } = Hex.API.get_registry
  end

  test "keys" do
    auth = [user: "user", pass: "hunter42"]
    assert { 201, body } = Hex.API.new_key("macbook", auth)
    assert { 201, _ } = Hex.API.new_package("melon", [], [key: body["secret"]])

    assert { 200, body } = Hex.API.get_keys(auth)
    key = Enum.find(body, &(&1["name"] == "macbook"))
    assert byte_size(key["secret"]) == 32

    assert { 204, _ } = Hex.API.delete_key("macbook", auth)

    assert { 200, body } = Hex.API.get_keys(auth)
    refute Enum.find(body, &(&1["name"] == "macbook"))
  end

  test "owners" do
    auth = [user: "user", pass: "hunter42"]
    Hex.API.new_package("orange", [], auth)
    Hex.API.new_user("orange_user", "orange_user@mail.com", "hunter42")

    assert {200, [%{"username" => "user"}]} = Hex.API.get_package_owners("orange", auth)

    assert {204, _} = Hex.API.add_package_owner("orange", "orange_user@mail.com", auth)

    assert {200, [%{"username" => "user"}, %{"username" => "orange_user"}]} =
           Hex.API.get_package_owners("orange", auth)

    assert {204, _} = Hex.API.delete_package_owner("orange", "orange_user@mail.com", auth)

    assert {200, [%{"username" => "user"}]} = Hex.API.get_package_owners("orange", auth)
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
