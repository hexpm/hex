defmodule Mix.Tasks.Hex.RegistryTest do
  use HexTest.Case

  test "build" do
    in_tmp(fn ->
      bypass = setup_bypass()

      Mix.Task.rerun("hex.registry", ~w(build --name foo))

      assert_received {:mix_shell, :info, ["* creating private_key.pem"]}
      assert_received {:mix_shell, :info, ["* creating public/public_key"]}
      assert_received {:mix_shell, :info, ["* creating public/tarballs"]}
      assert_received {:mix_shell, :info, ["* creating public/names"]}
      assert_received {:mix_shell, :info, ["* creating public/versions"]}
      refute_received _

      config = %{
        :mix_hex_core.default_config()
        | repo_url: "http://localhost:#{bypass.port}",
          repo_verify: false,
          repo_verify_origin: false
      }

      assert {:ok, {200, _, []}} = :mix_hex_repo.get_names(config)
      assert {:ok, {200, _, []}} = :mix_hex_repo.get_versions(config)

      {:ok, %{tarball: tarball}} = :mix_hex_tarball.create(%{name: "foo", version: "0.10.0"}, [])
      File.write!("public/tarballs/foo-0.10.0.tar", tarball)

      Mix.Task.rerun("hex.registry", ~w(build --name foo))
      assert_received {:mix_shell, :info, ["* using private_key.pem"]}
      assert_received {:mix_shell, :info, ["* creating public/packages/foo"]}
      assert_received {:mix_shell, :info, ["* updating public/names"]}
      assert_received {:mix_shell, :info, ["* updating public/versions"]}
      refute_received _

      assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
      assert names == [%{name: "foo"}]
      assert {:ok, {200, _, versions}} = :mix_hex_repo.get_versions(config)
      assert versions == [%{name: "foo", retired: [], versions: ["0.10.0"]}]

      {:ok, %{tarball: tarball}} = :mix_hex_tarball.create(%{name: "foo", version: "0.9.0"}, [])
      File.write!("public/tarballs/foo-0.9.0.tar", tarball)

      Mix.Task.rerun("hex.registry", ~w(build --name foo))
      assert_received {:mix_shell, :info, ["* using private_key.pem"]}
      assert_received {:mix_shell, :info, ["* updating public/packages/foo"]}
      assert_received {:mix_shell, :info, ["* updating public/names"]}
      assert_received {:mix_shell, :info, ["* updating public/versions"]}
      refute_received _

      assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
      assert names == [%{name: "foo"}]
      assert {:ok, {200, _, versions}} = :mix_hex_repo.get_versions(config)
      assert versions == [%{name: "foo", retired: [], versions: ["0.9.0", "0.10.0"]}]

      # Re-generating private key
      File.rm!("private_key.pem")
      Mix.Task.rerun("hex.registry", ~w(build --name foo))
      assert_received {:mix_shell, :info, ["* creating private_key.pem"]}
      assert_received {:mix_shell, :info, ["* public key at public/public_key does not" <> _]}
      assert_received {:mix_shell, :info, ["* updating public/public_key"]}
    end)
  end

  defp setup_bypass() do
    bypass = Bypass.open()

    Bypass.expect(bypass, fn conn ->
      opts = Plug.Static.init(at: "/", from: File.cwd!() <> "/public")
      Plug.Static.call(conn, opts)
    end)

    bypass
  end
end
