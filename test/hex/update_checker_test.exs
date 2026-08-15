defmodule Hex.UpdateCheckerTest do
  use HexTest.Case
  alias Hex.UpdateChecker

  defp bypass_csv(versions) do
    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)
    repos = put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}")
    Hex.State.put(:repos, repos)

    Bypass.expect(bypass, fn %Plug.Conn{request_path: "/installs/hex-1.x.csv"} = conn ->
      Plug.Conn.resp(conn, 200, versions_to_csv(versions))
    end)

    bypass
  end

  defp versions_to_csv(versions) do
    Enum.map_join(versions, "\n", fn {hex, elixir} ->
      "#{hex},DIGEST,#{elixir}"
    end)
  end

  setup do
    Hex.Registry.Server.open(check_version: false)
    Hex.Registry.Server.last_update({{2010, 1, 1}, {0, 0, 0}})
    :ok
  end

  test "display new hex version" do
    flush()
    bypass_csv([{"100.0.0", "1.0.0"}])

    {:ok, pid} = UpdateChecker.start_link(name: nil)
    GenServer.cast(pid, :start_check)
    assert {:version, _} = GenServer.call(pid, :check)
  end

  test "uses the global OAuth token for Hex.pm" do
    flush()

    Hex.OAuth.store_token(%{
      access_token: "hexpm-user-token",
      refresh_token: "hexpm-refresh-token",
      expires_at: System.system_time(:second) + 3600
    })

    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)
    repos = put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}")
    Hex.State.put(:repos, repos)

    Bypass.expect_once(bypass, "GET", "/installs/hex-1.x.csv", fn conn ->
      assert Plug.Conn.get_req_header(conn, "authorization") == ["Bearer hexpm-user-token"]
      Plug.Conn.resp(conn, 200, versions_to_csv([{"100.0.0", "1.0.0"}]))
    end)

    {:ok, pid} = UpdateChecker.start_link(name: nil)
    GenServer.cast(pid, :start_check)
    assert {:version, _} = GenServer.call(pid, :check)
  end

  test "dont display same hex version" do
    flush()
    bypass_csv([{"0.0.1", "1.0.0"}])

    {:ok, pid} = UpdateChecker.start_link(name: nil)
    GenServer.cast(pid, :start_check)
    assert :latest = GenServer.call(pid, :check)
  end

  test "dont display new hex version for too new elixir" do
    flush()
    bypass_csv([{"100.0.0", "100.0.0"}])

    {:ok, pid} = UpdateChecker.start_link(name: nil)
    GenServer.cast(pid, :start_check)
    assert :latest = GenServer.call(pid, :check)
  end

  test "only check version once" do
    flush()
    bypass_csv([{"100.0.0", "1.0.0"}])

    {:ok, pid} = UpdateChecker.start_link(name: nil)
    GenServer.cast(pid, :start_check)
    assert {:version, _} = GenServer.call(pid, :check)

    GenServer.cast(pid, :start_check)
    assert :already_checked = GenServer.call(pid, :check)
  end

  test "handle check timeout" do
    flush()

    init_state = %{started: true, check_timeout: 1}

    {:ok, pid} = UpdateChecker.start_link(name: nil, init_state: init_state)

    assert :timeout = GenServer.call(pid, :check)
  end
end
