defmodule Hex.APITest do
  use HexTest.Case
  @moduletag :integration

  test "user" do
    assert {:ok, {201, _, _}} = Hex.API.User.new("test_user", "test_user@mail.com", "hunter42")
    assert {:ok, {200, %{"username" => "test_user"}, _}} = Hex.API.User.get("test_user")
    assert {:ok, {404, _, _}} = Hex.API.User.get("unknown_user")
  end

  test "release" do
    auth = Hexpm.new_key([user: "user", pass: "hunter42"])

    meta = %{name: :pear, app: :pear, version: "0.0.1", build_tools: ["mix"], requirements: [], licenses: ["MIT"], description: "pear"}
    {tar, _checksum} = Hex.create_tar!(meta, [], :memory)
    assert {:ok, {404, _, _}} = Hex.API.Release.get("hexpm", "pear", "0.0.1")
    assert {:ok, {201, _, _}} = Hex.API.Release.new("hexpm", "pear", tar, auth)
    assert {:ok, {200, body, _}} = Hex.API.Release.get("hexpm", "pear", "0.0.1")
    assert body["requirements"] == %{}

    reqs = [%{name: :pear, app: :pear, requirement: "~> 0.0.1", optional: false}]
    meta = %{name: :grape, app: :grape, version: "0.0.2", build_tools: ["mix"], requirements: reqs, licenses: ["MIT"], description: "grape"}
    {tar, _checksum} = Hex.create_tar!(meta, [], :memory)
    assert {:ok, {201, _, _}} = Hex.API.Release.new("hexpm", "grape", tar, auth)
    assert {:ok, {200, body, _}} = Hex.API.Release.get("hexpm", "grape", "0.0.2")
    assert body["requirements"] == %{"pear" => %{"app" => "pear", "requirement" => "~> 0.0.1", "optional" => false}}

    assert {:ok, {204, _, _}} = Hex.API.Release.delete("hexpm", "grape", "0.0.2", auth)
    assert {:ok, {404, _, _}} = Hex.API.Release.get("hexpm", "grape", "0.0.2")
  end

  test "docs" do
    auth = Hexpm.new_key([user: "user", pass: "hunter42"])

    meta = %{name: :tangerine, app: :tangerine, version: "0.0.1", build_tools: ["mix"], requirements: [], licenses: ["MIT"], description: "tangerine"}
    {tar, _checksum} = Hex.create_tar!(meta, [], :memory)
    assert {:ok, {201, _, _}} = Hex.API.Release.new("hexpm", "tangerine", tar, auth)

    tarball = Path.join(tmp_path(), "docs.tar.gz")
    :ok = :hex_erl_tar.create(tarball, [{'index.html', "heya"}], [:compressed])
    tar = File.read!(tarball)

    assert {:ok, {201, _, _}} = Hex.API.ReleaseDocs.new("hexpm", "tangerine", "0.0.1", tar, auth)
    assert {:ok, {200, %{"has_docs" => true}, _}} = Hex.API.Release.get("hexpm", "tangerine", "0.0.1")

    assert {:ok, {204, _, _}} = Hex.API.ReleaseDocs.delete("hexpm", "tangerine", "0.0.1", auth)
    assert {:ok, {200, %{"has_docs" => false}, _}} = Hex.API.Release.get("hexpm", "tangerine", "0.0.1")
  end

  test "keys" do
    auth = [user: "user", pass: "hunter42"]

    assert {:ok, {201, %{"secret" => key_a}, _}} = Hex.API.Key.new("key_a", auth)
    assert {:ok, {201, %{"secret" => key_b}, _}} = Hex.API.Key.new("key_b", auth)
    assert byte_size(key_a) == 32
    assert byte_size(key_b) == 32
    auth = [key: key_a]

    Hexpm.new_package("melon", "0.0.1", %{}, %{}, auth)

    assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
    assert Enum.find(body, &(&1["name"] == "key_a"))

    assert {:ok, {200, _, _}} = Hex.API.Key.delete("key_b", auth)
                        assert {:ok, {200, body, _}} = Hex.API.Key.delete("key_a", auth)
    assert body["name"] == "key_a"
    assert {:ok, {401, _, _}} = Hex.API.Key.get(auth)

    # Delete all keys
    auth = [user: "user", pass: "hunter42"]
    assert {:ok, {201, %{"secret" => key_c}, _}} = Hex.API.Key.new("key_c", auth)
    assert {:ok, {201, %{"secret" => key_d}, _}} = Hex.API.Key.new("key_d", auth)
    assert byte_size(key_c) == 32
    assert byte_size(key_d) == 32
    auth_c = [key: key_c]
    auth_d = [key: key_d]

    assert {:ok, {200, body, _}} = Hex.API.Key.get(auth_c)
    assert Enum.find(body, &(&1["name"] == "key_c"))
    assert {:ok, {200, body, _}} = Hex.API.Key.get(auth_d)
    assert Enum.find(body, &(&1["name"] == "key_d"))

    assert {:ok, {200, body, _}} = Hex.API.Key.delete_all(auth_c)
    assert body["name"] == "key_c"
    assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_c)
    assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_d)
  end

  test "owners" do
    auth = Hexpm.new_key([user: "user", pass: "hunter42"])

    Hexpm.new_package("orange", "0.0.1", %{}, %{}, auth)
    Hex.API.User.new("orange_user", "orange_user@mail.com", "hunter42")

    assert {:ok, {200, [%{"username" => "user"}], _}} = Hex.API.Package.Owner.get("hexpm", "orange", auth)

    assert {:ok, {204, _, _}} = Hex.API.Package.Owner.add("hexpm", "orange", "orange_user@mail.com", auth)

    assert {:ok, {200, owners, _}} = Hex.API.Package.Owner.get("hexpm", "orange", auth)
    assert length(owners) == 2
    assert Enum.any?(owners, &match?(%{"username" => "user"}, &1))
    assert Enum.any?(owners, &match?(%{"username" => "orange_user"}, &1))

    assert {:ok, {204, _, _}} = Hex.API.Package.Owner.delete("hexpm", "orange", "orange_user@mail.com", auth)

    assert {:ok, {200, [%{"username" => "user"}], _}} = Hex.API.Package.Owner.get("hexpm", "orange", auth)
  end
end
