defmodule Hex.APITest do
  use HexTest.Case
  @moduletag :integration

  test "user" do
    assert {401, _, _} = Hex.API.User.get("test_user", [key: "something wrong"])
    assert {201, _, _} = Hex.API.User.new("test_user", "test_user@mail.com", "hunter42")

    auth = HexTest.HexWeb.new_key([user: "test_user", pass: "hunter42"])
    assert {200, %{"username" => "test_user"}, _} = Hex.API.User.get("test_user", auth)
  end

  test "release" do
    auth = HexTest.HexWeb.new_key([user: "user", pass: "hunter42"])

    meta = %{name: :pear, app: :pear, version: "0.0.1", build_tools: [], requirements: %{}, licenses: ["MIT"], description: "pear"}
    tar = Hex.Tar.create(meta, [])
    assert {404, _, _} = Hex.API.Release.get("pear", "0.0.1")
    assert {201, _, _} = Hex.API.Release.new("pear", tar, auth)
    assert {200, body, _} = Hex.API.Release.get("pear", "0.0.1")
    assert body["requirements"] == %{}

    meta = %{name: :grape, app: :grape, version: "0.0.2", build_tools: [], requirements: %{pear: %{requirement: "~> 0.0.1"}}, licenses: ["MIT"], description: "grape"}
    tar = Hex.Tar.create(meta, [])
    assert {201, _, _} = Hex.API.Release.new("grape", tar, auth)
    assert {200, body, _} = Hex.API.Release.get("grape", "0.0.2")
    assert body["requirements"] == %{"pear" => %{"app" => "pear", "requirement" => "~> 0.0.1", "optional" => false}}

    assert {204, _, _} = Hex.API.Release.delete("grape", "0.0.2", auth)
    assert {404, _, _} = Hex.API.Release.get("grape", "0.0.2")
  end

  test "docs" do
    auth = HexTest.HexWeb.new_key([user: "user", pass: "hunter42"])

    meta = %{name: :tangerine, app: :tangerine, version: "0.0.1", build_tools: [], requirements: %{}, licenses: ["MIT"], description: "tangerine"}
    tar = Hex.Tar.create(meta, [])
    assert {201, _, _} = Hex.API.Release.new("tangerine", tar, auth)

    tarball = Path.join(tmp_path, "docs.tar.gz")
    :ok = :erl_tar.create(tarball, [{'index.html', "heya"}], [:compressed])
    tar = File.read!(tarball)

    assert {201, _, _} = Hex.API.ReleaseDocs.new("tangerine", "0.0.1", tar, auth)
    assert {200, %{"has_docs" => true}, _} = Hex.API.Release.get("tangerine", "0.0.1")

    assert {204, _, _} = Hex.API.ReleaseDocs.delete("tangerine", "0.0.1", auth)
    assert {200, %{"has_docs" => false}, _} = Hex.API.Release.get("tangerine", "0.0.1")
  end

  test "registry" do
    assert {200, _, _} = Hex.API.Registry.get
  end

  test "keys" do
    auth = [user: "user", pass: "hunter42"]

    assert {201, %{"secret" => key}, _} = Hex.API.Key.new("macbook", auth)
    assert byte_size(key) == 32
    auth = [key: key]

    HexTest.HexWeb.new_package("melon", "0.0.1", %{}, %{}, auth)

    assert {200, body, _} = Hex.API.Key.get(auth)
    assert Enum.find(body, &(&1["name"] == "macbook"))

    assert {204, _, _} = Hex.API.Key.delete("macbook", auth)
    assert {401, _, _} = Hex.API.Key.get(auth)
  end

  test "owners" do
    auth = HexTest.HexWeb.new_key([user: "user", pass: "hunter42"])

    HexTest.HexWeb.new_package("orange", "0.0.1", %{}, %{}, auth)
    Hex.API.User.new("orange_user", "orange_user@mail.com", "hunter42")

    assert {200, [%{"username" => "user"}], _} = Hex.API.Package.Owner.get("orange", auth)

    assert {204, _, _} = Hex.API.Package.Owner.add("orange", "orange_user@mail.com", auth)

    assert {200, owners, _} = Hex.API.Package.Owner.get("orange", auth)
    assert length(owners) == 2
    assert Enum.any?(owners, &match?(%{"username" => "user"}, &1))
    assert Enum.any?(owners, &match?(%{"username" => "orange_user"}, &1))

    assert {204, _, _} = Hex.API.Package.Owner.delete("orange", "orange_user@mail.com", auth)

    assert {200, [%{"username" => "user"}], _} = Hex.API.Package.Owner.get("orange", auth)
  end

  test "x-hex-message" do
    Hex.API.Utils.handle_hex_message('"oops, you done goofed"')
    refute_received {:mix_shell, _, _}

    Hex.API.Utils.handle_hex_message('  "oops, you done goofed" ; level = warn')
    assert_received {:mix_shell, :info, ["API warning: oops, you done goofed"]}

    Hex.API.Utils.handle_hex_message('"oops, you done goofed";level=fatal  ')
    assert_received {:mix_shell, :error, ["API error: oops, you done goofed"]}
  end
end
