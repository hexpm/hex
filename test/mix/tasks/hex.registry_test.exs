defmodule Mix.Tasks.Hex.RegistryTest do
  use HexTest.Case

  describe "add" do
    test "adds a single package to an empty registry" do
      in_tmp(fn ->
        bypass = setup_bypass()

        0 = Mix.shell().cmd("openssl genrsa -out private_key.pem")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        flush()

        {:ok, %{tarball: tarball}} =
          :mix_hex_tarball.create(%{name: "foo", version: "0.10.0"}, [])

        File.mkdir!("subdir")
        File.write!("subdir/foo-0.10.0.tar", tarball)
        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(add public --name acme --private-key private_key.pem subdir/foo-0.10.0.tar)
        )

        assert_received {:mix_shell, :info,
                         ["* copying subdir/foo-0.10.0.tar -> public/tarballs/foo-0.10.0.tar"]}

        assert_received {:mix_shell, :info, ["* creating public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _

        config = %{
          :mix_hex_core.default_config()
          | repo_url: "http://localhost:#{bypass.port}",
            repo_verify: false,
            repo_verify_origin: false
        }

        assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
        assert [%{name: "foo", updated_at: %{seconds: updated_at}}] = names

        assert updated_at ==
                 "public/tarballs/foo-0.10.0.tar"
                 |> File.stat!()
                 |> Map.fetch!(:mtime)
                 |> Mix.Tasks.Hex.Registry.to_unix()

        assert {:ok, {200, _, versions}} = :mix_hex_repo.get_versions(config)
        assert versions == [%{name: "foo", retired: [], versions: ["0.10.0"]}]
      end)
    end

    test "adds a single package to a populated registry" do
      in_tmp(fn ->
        bypass = setup_bypass()

        {:ok, %{tarball: tarball}} =
          :mix_hex_tarball.create(%{name: "foo", version: "0.10.0"}, [])

        File.mkdir_p!("public/tarballs")
        File.write!("public/tarballs/foo-0.10.0.tar", tarball)
        0 = Mix.shell().cmd("openssl genrsa -out private_key.pem")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        flush()

        {:ok, %{tarball: tarball}} = :mix_hex_tarball.create(%{name: "foo", version: "0.9.0"}, [])
        File.mkdir!("subdir")
        File.write!("subdir/foo-0.9.0.tar", tarball)

        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(add public --name acme --private-key private_key.pem subdir/foo-0.9.0.tar)
        )

        assert_received {:mix_shell, :info,
                         ["* copying subdir/foo-0.9.0.tar -> public/tarballs/foo-0.9.0.tar"]}

        assert_received {:mix_shell, :info, ["* updating public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _

        config = %{
          :mix_hex_core.default_config()
          | repo_url: "http://localhost:#{bypass.port}",
            repo_verify: false,
            repo_verify_origin: false
        }

        assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
        assert [%{name: "foo", updated_at: %{seconds: updated_at}}] = names

        assert updated_at ==
                 "public/tarballs/foo-0.9.0.tar"
                 |> File.stat!()
                 |> Map.fetch!(:mtime)
                 |> Mix.Tasks.Hex.Registry.to_unix()

        assert {:ok, {200, _, versions}} = :mix_hex_repo.get_versions(config)
        assert versions == [%{name: "foo", retired: [], versions: ["0.9.0", "0.10.0"]}]
      end)
    end
  end

  describe "build" do
    test "builds a registry" do
      in_tmp(fn ->
        bypass = setup_bypass()

        0 = Mix.shell().cmd("openssl genrsa -out private_key.pem")
        flush()

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

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

        {:ok, %{tarball: tarball}} =
          :mix_hex_tarball.create(%{name: "foo", version: "0.10.0"}, [])

        File.write!("public/tarballs/foo-0.10.0.tar", tarball)

        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        assert_received {:mix_shell, :info, ["* creating public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _

        assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
        assert [%{name: "foo", updated_at: %{seconds: updated_at}}] = names

        assert updated_at ==
                 "public/tarballs/foo-0.10.0.tar"
                 |> File.stat!()
                 |> Map.fetch!(:mtime)
                 |> Mix.Tasks.Hex.Registry.to_unix()

        assert {:ok, {200, _, versions}} = :mix_hex_repo.get_versions(config)
        assert versions == [%{name: "foo", retired: [], versions: ["0.10.0"]}]

        {:ok, %{tarball: tarball}} = :mix_hex_tarball.create(%{name: "foo", version: "0.9.0"}, [])
        File.write!("public/tarballs/foo-0.9.0.tar", tarball)

        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        assert_received {:mix_shell, :info, ["* updating public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _

        assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
        assert [%{name: "foo", updated_at: %{seconds: updated_at}}] = names

        assert updated_at ==
                 "public/tarballs/foo-0.9.0.tar"
                 |> File.stat!()
                 |> Map.fetch!(:mtime)
                 |> Mix.Tasks.Hex.Registry.to_unix()

        assert {:ok, {200, _, versions}} = :mix_hex_repo.get_versions(config)
        assert versions == [%{name: "foo", retired: [], versions: ["0.9.0", "0.10.0"]}]

        # Versions with hyphen
        {:ok, %{tarball: tarball}} =
          :mix_hex_tarball.create(%{name: "foo", version: "1.0.0-rc"}, [])

        File.write!("public/tarballs/foo-1.0.0-rc.tar", tarball)

        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        assert_received {:mix_shell, :info, ["* updating public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _

        assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
        assert [%{name: "foo", updated_at: _}] = names
        assert {:ok, {200, _, versions}} = :mix_hex_repo.get_versions(config)

        assert versions == [
                 %{name: "foo", retired: [], versions: ["0.9.0", "0.10.0", "1.0.0-rc"]}
               ]

        # Re-generating private key
        0 = Mix.shell().cmd("openssl genrsa -out private_key.pem")
        flush()
        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        assert_received {:mix_shell, :info, ["* public key at public/public_key does not" <> _]}
        assert_received {:mix_shell, :info, ["* updating public/public_key"]}
        assert_received {:mix_shell, :info, ["* updating public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _

        # Package with deps
        metadata = %{
          name: "bar",
          version: "0.1.0",
          requirements: %{
            "foo" => %{
              "app" => "foo",
              "optional" => false,
              "repository" => "acme",
              "requirement" => "~> 0.1.0"
            },
            "baz" => %{
              "app" => "baz",
              "optional" => false,
              "repository" => "external",
              "requirement" => "~> 0.1.0"
            }
          }
        }

        {:ok, %{tarball: tarball}} = :mix_hex_tarball.create(metadata, [])
        File.write!("public/tarballs/bar-0.1.0.tar", tarball)

        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        assert_received {:mix_shell, :info, ["* creating public/packages/bar"]}
        assert_received {:mix_shell, :info, ["* updating public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _

        assert {:ok, {200, _, names}} = :mix_hex_repo.get_names(config)
        assert [%{name: "bar", updated_at: _}, %{name: "foo", updated_at: _}] = names
        assert {:ok, {200, _, [package]}} = :mix_hex_repo.get_package(config, "bar")

        assert package.dependencies == [
                 %{
                   app: "baz",
                   optional: false,
                   package: "baz",
                   requirement: "~> 0.1.0",
                   repository: "external"
                 },
                 %{
                   app: "foo",
                   optional: false,
                   package: "foo",
                   requirement: "~> 0.1.0"
                 }
               ]

        # Removing all package releases
        File.rm!("public/tarballs/foo-0.9.0.tar")
        File.rm!("public/tarballs/foo-0.10.0.tar")
        File.rm!("public/tarballs/foo-1.0.0-rc.tar")
        Mix.Task.reenable("hex.registry")

        Mix.Task.run(
          "hex.registry",
          ~w(build public --name acme --private-key private_key.pem)
        )

        assert_received {:mix_shell, :info, ["* updating public/packages/bar"]}
        assert_received {:mix_shell, :info, ["* removing public/packages/foo"]}
        assert_received {:mix_shell, :info, ["* updating public/names"]}
        assert_received {:mix_shell, :info, ["* updating public/versions"]}
        refute_received _
      end)
    end
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
