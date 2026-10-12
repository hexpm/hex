defmodule Mix.Tasks.Hex.PackageMirrorTest do
  use HexTest.Case
  import ExUnit.CaptureIO

  setup do
    Mix.shell(Mix.Shell.IO)
    on_exit(fn -> Mix.shell(Hex.Shell.Process) end)
    :ok
  end

  defp foo_tarball(contents) do
    metadata = %{
      "name" => "foo",
      "version" => "1.0.0",
      "app" => "foo",
      "build_tools" => ["mix"],
      "requirements" => [],
      "files" => ["lib/foo.ex"]
    }

    files = [{~c"lib/foo.ex", contents}]
    {:ok, tarball} = :mix_hex_tarball.create(metadata, files, :mix_hex_core.default_config())
    tarball
  end

  defp foo_registry(%{inner_checksum: inner_checksum, outer_checksum: outer_checksum}) do
    release = %{
      version: "1.0.0",
      inner_checksum: inner_checksum,
      outer_checksum: outer_checksum,
      dependencies: []
    }

    payload =
      :mix_hex_registry.encode_package(%{repository: "hexpm", name: "foo", releases: [release]})

    private_key = File.read!(fixture_path("test_priv.pem"))
    :zlib.gzip(:mix_hex_registry.sign_protobuf(payload, private_key))
  end

  defp mirror(routes, headers \\ []) do
    bypass = Bypass.open()
    Hex.State.put(:mirror_url, "http://localhost:#{bypass.port}")
    test = self()

    Bypass.expect(bypass, fn conn ->
      send(test, {:mirror_request, conn.request_path})
      {status, body} = Map.get(routes, conn.request_path, {404, ""})

      conn
      |> Plug.Conn.merge_resp_headers(headers)
      |> Plug.Conn.resp(status, body)
    end)
  end

  defp capture_stdout_and_stderr(fun) do
    stderr =
      capture_io(:stderr, fn ->
        stdout = capture_io([encoding: :latin1], fun)
        send(self(), {:stdout, stdout})
      end)

    assert_received {:stdout, stdout}
    {stdout, stderr}
  end

  defp fetch_to_stdout() do
    Mix.Tasks.Hex.Package.run(["fetch", "foo", "1.0.0", "--output", "-"])
  end

  test "fetch: to stdout prints registry errors to stderr" do
    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      package = foo_tarball("defmodule Foo do end")

      mirror(%{
        "/packages/foo" => {500, "body of the mirror's error response"},
        "/tarballs/foo-1.0.0.tar" => {200, package.tarball}
      })

      {stdout, stderr} =
        capture_stdout_and_stderr(fn ->
          assert_raise Mix.Error, ~r/Checksum mismatch against registry \(outer\)/, fn ->
            fetch_to_stdout()
          end
        end)

      assert stdout == ""
      assert stderr =~ "Failed to fetch record for foo from registry"
      assert stderr =~ "body of the mirror's error response"
    end)
  end

  test "fetch: to stdout prints API warnings to stderr" do
    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      package = foo_tarball("defmodule Foo do end")

      mirror(
        %{
          "/packages/foo" => {200, foo_registry(package)},
          "/tarballs/foo-1.0.0.tar" => {200, package.tarball}
        },
        [{"x-hex-message", ~s("message from the mirror"; level=warn)}]
      )

      {stdout, stderr} = capture_stdout_and_stderr(&fetch_to_stdout/0)

      assert stdout == package.tarball
      assert stderr =~ "API warning: message from the mirror"
    end)
  end

  test "fetch: to stdout doesn't check for Hex updates" do
    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      package = foo_tarball("defmodule Foo do end")

      mirror(%{
        "/packages/foo" => {200, foo_registry(package)},
        "/tarballs/foo-1.0.0.tar" => {200, package.tarball},
        "/installs/hex-1.x.csv" => {404, "body of the mirror's error response"}
      })

      {stdout, stderr} =
        capture_stdout_and_stderr(fn ->
          fetch_to_stdout()
          # Waits for an update check if the task started one
          Hex.UpdateChecker.check()
        end)

      assert stdout == package.tarball
      refute stderr =~ "Failed to check for new Hex version"
      refute_received {:mirror_request, "/installs/hex-1.x.csv"}
    end)
  end

  test "fetch: to stdout fetches the registry again after rejecting a tarball" do
    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      package = foo_tarball("defmodule Foo do end")
      republished = foo_tarball("defmodule Foo do def foo, do: :ok end")

      mirror(%{
        "/packages/foo" => {200, foo_registry(package)},
        "/tarballs/foo-1.0.0.tar" => {200, republished.tarball}
      })

      {stdout, _stderr} =
        capture_stdout_and_stderr(fn ->
          assert_raise Mix.Error, ~r/Checksum mismatch against registry \(outer\)/, fn ->
            fetch_to_stdout()
          end
        end)

      assert stdout == ""

      mirror(%{
        "/packages/foo" => {200, foo_registry(republished)},
        "/tarballs/foo-1.0.0.tar" => {200, republished.tarball}
      })

      {stdout, _stderr} = capture_stdout_and_stderr(&fetch_to_stdout/0)
      assert stdout == republished.tarball
    end)
  end

  test "fetch: to folder fetches the registry again after rejecting a tarball" do
    in_tmp(fn ->
      cwd = File.cwd!()
      Hex.State.put(:cache_home, cwd)
      package = foo_tarball("defmodule Foo do end")
      republished = foo_tarball("defmodule Foo do def foo, do: :ok end")
      args = ["fetch", "foo", "1.0.0", "--output", "#{cwd}/out"]

      # Skips the update check, which could finish after the task closed the registry
      Hex.Registry.Server.open(check_version: false)
      Hex.Registry.Server.last_update(:calendar.universal_time())
      Hex.Registry.Server.close()

      mirror(%{
        "/packages/foo" => {200, foo_registry(package)},
        "/tarballs/foo-1.0.0.tar" => {200, republished.tarball}
      })

      assert_raise Mix.Error, ~r/Checksum mismatch against registry \(outer\)/, fn ->
        Mix.Tasks.Hex.Package.run(args)
      end

      mirror(%{
        "/packages/foo" => {200, foo_registry(republished)},
        "/tarballs/foo-1.0.0.tar" => {200, republished.tarball}
      })

      assert capture_io(fn -> Mix.Tasks.Hex.Package.run(args) end) =~
               "foo v1.0.0 downloaded to #{cwd}/out/foo-1.0.0.tar"

      assert File.read!("#{cwd}/out/foo-1.0.0.tar") == republished.tarball
    end)
  end
end
