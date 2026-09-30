defmodule Mix.Tasks.Hex.PublishOwnerPromptTest do
  use HexTest.Case

  @package "publish_owner_prompt"

  setup do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    Hex.State.put(:api_key, "api_key")
    Process.put(:hex_test_app_name, String.to_atom(@package))
    Mix.Project.push(ReleaseSimple.MixProject)
    on_exit(fn -> purge([ReleaseSimple.MixProject]) end)
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

  defp stub_api(bypass, package_status: package_status) do
    test_pid = self()

    Bypass.expect(bypass, fn conn ->
      {:ok, _body, conn} = Plug.Conn.read_body(conn, length: 100_000_000)
      send(test_pid, {:request, conn.method, conn.request_path})

      case {conn.method, conn.request_path} do
        {"GET", "/api/users/me"} ->
          erlang_resp(conn, 200, %{"organizations" => [%{"name" => "acme"}]})

        {"GET", "/api/repos/hexpm/packages/" <> _name} ->
          erlang_resp(conn, package_status, %{})

        {"POST", "/api/packages/" <> _path} ->
          erlang_resp(conn, 201, %{"html_url" => "http://hex/#{@package}"})
      end
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end
end
