defmodule Hex.RepoTest do
  use HexTest.IntegrationCase

  @private_key File.read!(Path.join(__DIR__, "../fixtures/test_priv.pem"))

  test "get_package/3" do
    assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

    assert_raise Mix.Error, ~r"Unknown repository \"bad\"", fn ->
      Hex.Repo.get_package("bad", "postgrex", "")
    end
  end

  test "get_tarball/3" do
    assert {:ok, {200, _, _}} = Hex.Repo.get_tarball("hexpm", "postgrex", "0.2.1")

    assert_raise Mix.Error, ~r"Unknown repository \"bad\"", fn ->
      Hex.Repo.get_tarball("bad", "postgrex", "0.2.1")
    end
  end

  test "verify signature" do
    message = :mix_hex_registry.sign_protobuf("payload", @private_key)
    assert Hex.Repo.verify(message, "hexpm") == "payload"

    assert_raise(Mix.Error, fn ->
      message = :mix_hex_pb_signed.encode_msg(%{payload: "payload", signature: "foobar"}, :Signed)
      Hex.Repo.verify(message, "hexpm")
    end)
  end

  test "decode package" do
    package = %{releases: [], repository: "hexpm", name: "ecto"}
    message = :mix_hex_pb_package.encode_msg(package, :Package)

    assert Hex.Repo.decode_package(message, "hexpm", "ecto") == []
  end

  test "decode package verify origin" do
    package = %{releases: [], repository: "hexpm", name: "ecto"}
    message = :mix_hex_pb_package.encode_msg(package, :Package)

    assert_raise(Mix.Error, fn ->
      Hex.Repo.decode_package(message, "other repo", "ecto")
    end)

    assert_raise(Mix.Error, fn ->
      Hex.Repo.decode_package(message, "hexpm", "other package")
    end)

    Hex.State.put(:no_verify_repo_origin, true)

    assert Hex.Repo.decode_package(message, "other repo", "ecto") == %{
             name: "ecto",
             releases: [],
             repository: "hexpm"
           }

    assert Hex.Repo.decode_package(message, "hexpm", "other package") == %{
             name: "ecto",
             releases: [],
             repository: "hexpm"
           }
  end

  test "get public key" do
    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)
    hexpm = Hex.Repo.hexpm_repo()
    repos = put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}")
    Hex.State.put(:repos, repos)

    Bypass.expect(bypass, fn %Plug.Conn{request_path: path} = conn ->
      case path do
        "/public_key" ->
          assert Plug.Conn.get_req_header(conn, "authorization") == ["key"]
          Plug.Conn.resp(conn, 200, hexpm.public_key)

        "/not_found/public_key" ->
          assert Plug.Conn.get_req_header(conn, "authorization") == []
          Plug.Conn.resp(conn, 404, "not found")
      end
    end)

    config = %{url: "http://localhost:#{bypass.port}", auth_key: "key", trusted: true}
    assert {:ok, {200, public_key, _}} = Hex.Repo.get_public_key(config)
    assert public_key == hexpm.public_key

    config = %{url: "http://localhost:#{bypass.port}/not_found", auth_key: "key", trusted: false}
    assert {:ok, {404, "not found", _}} = Hex.Repo.get_public_key(config)
  end

  test "fetch_repo/1" do
    assert Hex.Repo.fetch_repo("foo") == :error

    assert {:ok,
            %{
              auth_key: nil,
              public_key: _,
              trusted: true,
              url: "http://localhost:4043/repo"
            }} = Hex.Repo.fetch_repo("hexpm")

    assert {:ok,
            %{
              auth_key: nil,
              public_key: _,
              trusted: true,
              url: "http://localhost:4043/repo/repos/acme"
            }} = Hex.Repo.fetch_repo("hexpm:acme")

    Hex.State.put(:trusted_mirror_url, "http://example.com")
    Hex.State.put(:repos_key, "key")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: true,
              url: "http://example.com"
            }} = Hex.Repo.fetch_repo("hexpm")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: true,
              url: "http://example.com/repos/acme"
            }} = Hex.Repo.fetch_repo("hexpm:acme")

    Hex.State.put(:trusted_mirror_url, nil)
    Hex.State.put(:mirror_url, "http://example.com")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: false,
              url: "http://example.com"
            }} = Hex.Repo.fetch_repo("hexpm")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: false,
              url: "http://example.com/repos/acme"
            }} = Hex.Repo.fetch_repo("hexpm:acme")
  end

  if Version.match?(System.version(), "< 1.6.0") do
    @tag :skip
  end

  test "update_organizations/1 without Hex.State" do
    :ok = Supervisor.terminate_child(Hex.Supervisor, Hex.State)
    :ok = Supervisor.delete_child(Hex.Supervisor, Hex.State)

    repos = %{
      "hexpm" => %{
        url: "http://example.com",
        public_key: "public",
        auth_key: "auth",
        trusted: true
      },
      "hexpm:acme" => %{}
    }

    assert %{
             "hexpm:acme" => %{
               auth_key: "auth",
               public_key: "public",
               trusted: true,
               url: "http://example.com/repos/acme"
             }
           } = Hex.Repo.update_organizations(repos)
  after
    {:ok, _} = Supervisor.start_child(Hex.Supervisor, Hex.State)
  end
end
