defmodule Hex.APITest do
  use HexTest.IntegrationCase

  test "user" do
    assert {:ok, {201, _, _}} = Hex.API.User.new("test_user", "test_user@mail.com", "hunter42")
    assert {:ok, {200, _, %{"username" => "test_user"}}} = Hex.API.User.get("test_user")
    assert {:ok, {404, _, _}} = Hex.API.User.get("unknown_user")
  end

  defp meta(name, version, requirements) do
    %{
      name: name,
      app: name,
      version: version,
      build_tools: ["mix"],
      requirements: requirements,
      licenses: ["MIT"],
      description: "description",
      files: ["mix.exs"]
    }
  end

  test "release" do
    auth = Hexpm.new_key(user: "user", pass: "hunter42")

    %{tarball: tarball} = Hex.Tar.create!(meta(:pear, "0.0.1", []), ["mix.exs"], :memory)
    assert {:ok, {404, _, _}} = Hex.API.Release.get("hexpm", "pear", "0.0.1")
    assert {:ok, {201, _, _}} = Hex.API.Release.publish("hexpm", tarball, auth)
    assert {:ok, {200, _, body}} = Hex.API.Release.get("hexpm", "pear", "0.0.1")
    assert body["requirements"] == %{}

    reqs = [%{name: :pear, app: :pear, requirement: "~> 0.0.1", optional: false}]

    %{tarball: tarball} = Hex.Tar.create!(meta(:grape, "0.0.2", reqs), ["mix.exs"], :memory)
    assert {:ok, {201, _, _}} = Hex.API.Release.publish("hexpm", tarball, auth)
    assert {:ok, {200, _, body}} = Hex.API.Release.get("hexpm", "grape", "0.0.2")

    assert body["requirements"] == %{
             "pear" => %{"app" => "pear", "requirement" => "~> 0.0.1", "optional" => false}
           }

    assert {:ok, {status, _, _}} = Hex.API.Release.delete("hexpm", "grape", "0.0.2", auth)
    assert status in 200..299
    assert {:ok, {404, _, _}} = Hex.API.Release.get("hexpm", "grape", "0.0.2")
  end

  test "docs" do
    auth = Hexpm.new_key(user: "user", pass: "hunter42")

    %{tarball: tarball} = Hex.Tar.create!(meta(:tangerine, "0.0.1", []), ["mix.exs"], :memory)
    assert {:ok, {201, _, _}} = Hex.API.Release.publish("hexpm", tarball, auth)

    tarball = Path.join(tmp_path(), "docs.tar.gz")
    :ok = :mix_hex_erl_tar.create(tarball, [{~c"index.html", "heya"}], [:compressed])
    tarball = File.read!(tarball)

    assert {:ok, {201, _, _}} =
             Hex.API.ReleaseDocs.publish("hexpm", "tangerine", "0.0.1", tarball, auth)

    assert {:ok, {200, _, %{"has_docs" => true}}} =
             Hex.API.Release.get("hexpm", "tangerine", "0.0.1")

    assert {:ok, {status, _, _}} = Hex.API.ReleaseDocs.delete("hexpm", "tangerine", "0.0.1", auth)
    assert status in 200..299

    assert {:ok, {200, _, %{"has_docs" => false}}} =
             Hex.API.Release.get("hexpm", "tangerine", "0.0.1")
  end

  test "keys" do
    permissions = [%{"domain" => "api"}]
    auth = [user: "user", pass: "hunter42"]

    assert {:ok, {201, _, %{"secret" => key_a}}} = Hex.API.Key.new("key_a", permissions, auth)
    assert {:ok, {201, _, %{"secret" => key_b}}} = Hex.API.Key.new("key_b", permissions, auth)
    assert byte_size(key_a) == 32
    assert byte_size(key_b) == 32
    auth = [key: key_a]

    Hexpm.new_package("hexpm", "melon", "0.0.1", %{}, %{}, auth)

    assert {:ok, {200, _, body}} = Hex.API.Key.get(auth)
    assert Enum.find(body, &(&1["name"] == "key_a"))

    assert {:ok, {200, _, _}} = Hex.API.Key.delete("key_b", auth)
    assert {:ok, {200, _, body}} = Hex.API.Key.delete("key_a", auth)
    assert body["name"] == "key_a"
    assert {:ok, {401, _, _}} = Hex.API.Key.get(auth)

    # Delete all keys
    auth = [user: "user", pass: "hunter42"]
    assert {:ok, {201, _, %{"secret" => key_c}}} = Hex.API.Key.new("key_c", permissions, auth)
    assert {:ok, {201, _, %{"secret" => key_d}}} = Hex.API.Key.new("key_d", permissions, auth)
    assert byte_size(key_c) == 32
    assert byte_size(key_d) == 32
    auth_c = [key: key_c]
    auth_d = [key: key_d]

    assert {:ok, {200, _, body}} = Hex.API.Key.get(auth_c)
    assert Enum.find(body, &(&1["name"] == "key_c"))
    assert {:ok, {200, _, body}} = Hex.API.Key.get(auth_d)
    assert Enum.find(body, &(&1["name"] == "key_d"))

    assert {:ok, {200, _, body}} = Hex.API.Key.delete_all(auth_c)
    assert body["name"] == "key_c"
    assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_c)
    assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_d)
  end

  test "owners" do
    auth = Hexpm.new_key(user: "user", pass: "hunter42")

    Hexpm.new_package("hexpm", "orange", "0.0.1", %{}, %{}, auth)
    Hex.API.User.new("orange_user", "orange_user@mail.com", "hunter42")

    assert {:ok, {200, _, [%{"username" => "user"}]}} =
             Hex.API.Package.Owner.get("hexpm", "orange", auth)

    assert {:ok, {status, _, _}} =
             Hex.API.Package.Owner.add(
               "hexpm",
               "orange",
               "orange_user@mail.com",
               "full",
               false,
               auth
             )

    assert status in 200..299

    assert {:ok, {200, _, owners}} = Hex.API.Package.Owner.get("hexpm", "orange", auth)
    assert length(owners) == 2
    assert Enum.any?(owners, &match?(%{"username" => "user"}, &1))
    assert Enum.any?(owners, &match?(%{"username" => "orange_user"}, &1))

    assert {:ok, {status, _, _}} =
             Hex.API.Package.Owner.delete("hexpm", "orange", "orange_user@mail.com", auth)

    assert status in 200..299

    assert {:ok, {200, _, [%{"username" => "user"}]}} =
             Hex.API.Package.Owner.get("hexpm", "orange", auth)
  end
end
