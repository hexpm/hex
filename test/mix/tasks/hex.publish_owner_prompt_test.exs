defmodule Mix.Tasks.Hex.PublishOwnerPromptTest do
  use HexTest.Case

  @package "publish_owner_prompt"

  defmodule ReleaseDocs.MixProject do
    def project do
      [
        app: :publish_owner_prompt,
        description: "baz",
        version: "0.0.1",
        aliases: [docs: [&docs/1]],
        package: [licenses: ["MIT"], files: ["myfile.txt"], links: %{"a" => "http://a"}]
      ]
    end

    defp docs(_) do
      File.mkdir_p!("doc")
      File.write!("doc/index.html", "the index")
    end
  end

  setup do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    Hex.State.put(:api_key, "api_key")
    Mix.Project.push(ReleaseDocs.MixProject)
    {:ok, bypass: bypass}
  end

  test "--yes publishes without looking up the user", %{bypass: bypass} do
    stub_api(bypass, package_status: 200)

    in_tmp(fn ->
      File.write!("myfile.txt", "hello")
      Mix.Tasks.Hex.Publish.run(["package", "--yes", "--no-progress"])
    end)

    assert_received {:request, "POST", "/api/packages/#{@package}/releases"}
    refute_received {:request, _method, "/api/users/me"}
    refute_received {:request, _method, "/api/repos/hexpm/packages/#{@package}"}
  end

  test "an existing package publishes without looking up the user", %{bypass: bypass} do
    stub_api(bypass, package_status: 200)

    in_tmp(fn ->
      File.write!("myfile.txt", "hello")
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
    end)

    assert_received {:request, "GET", "/api/repos/hexpm/packages/#{@package}"}
    assert_received {:authorization, "/api/repos/hexpm/packages/#{@package}", []}
    assert_received {:request, "POST", "/api/packages/#{@package}/releases"}
    refute_received {:request, _method, "/api/users/me"}
  end

  test "a new package offers the user's organizations as owner", %{bypass: bypass} do
    stub_api(bypass, package_status: 404)

    in_tmp(fn ->
      File.write!("myfile.txt", "hello")
      send(self(), {:mix_shell_input, :prompt, "1"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
    end)

    assert_received {:request, "GET", "/api/users/me"}
    assert_received {:mix_shell, :info, ["  [2] acme"]}
    assert_received {:request, "POST", "/api/packages/#{@package}/releases"}
  end

  for {name, args} <- [{"package", ["package"]}, {"package and docs", []}] do
    test "a staged #{name} publish leaves the organization transfer for after approval", %{
      bypass: bypass
    } do
      stub_api(bypass,
        package_status: 404,
        release: %{
          "status" => "staged",
          "id" => 42,
          "kind" => "release",
          "checksum" => "package-checksum",
          "approval_checksum" => "package-and-docs-checksum",
          "html_url" => "https://hex.pm/dashboard/staged-releases/42",
          "expires_at" => "2026-11-01T12:00:00Z"
        }
      )

      in_tmp(fn ->
        File.write!("myfile.txt", "hello")
        send(self(), {:mix_shell_input, :prompt, "2"})
        Mix.Tasks.Hex.Publish.run(unquote(args) ++ ["--no-progress"])
      end)

      refute_received {:request, "PUT", _}
      assert_received {:mix_shell, :info, ["Ownership can be transferred to acme after approval"]}
    end
  end

  defp stub_api(bypass, opts) do
    test_pid = self()
    package_status = Keyword.fetch!(opts, :package_status)
    release = Keyword.get(opts, :release, %{"html_url" => "http://hex/#{@package}"})

    Bypass.expect(bypass, fn conn ->
      {:ok, _body, conn} = Plug.Conn.read_body(conn, length: 100_000_000)
      send(test_pid, {:request, conn.method, conn.request_path})

      send(
        test_pid,
        {:authorization, conn.request_path, Plug.Conn.get_req_header(conn, "authorization")}
      )

      case {conn.method, conn.request_path} do
        {"GET", "/api/users/me"} ->
          erlang_resp(conn, 200, %{"organizations" => [%{"name" => "acme"}]})

        {"GET", "/api/repos/hexpm/packages/" <> _name} ->
          erlang_resp(conn, package_status, %{})

        {"POST", "/api/packages/" <> _path} ->
          erlang_resp(conn, 201, release)
      end
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end
end
