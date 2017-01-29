defmodule Hex.RegistryTest do
  use HexTest.Case
  alias Hex.Registry.Server, as: Registry

  defp bypass_csv(versions) do
    bypass = Bypass.open
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

  test "display new hex version" do
    flush()
    bypass_csv([{"100.0.0", "1.0.0"}])

    Registry.open(registry_path: tmp_path(test_name() <> ".ets"))
    Registry.close
    assert_received {:mix_shell, :info, ["\e[33mA new Hex version is available" <> _]}
  end

  test "dont display same hex version" do
    flush()
    bypass_csv([{"0.0.1", "1.0.0"}])

    Registry.open(registry_path: tmp_path(test_name() <> ".ets"))
    Registry.close
    refute_received {:mix_shell, :info, ["\e[33mA new Hex version is available" <> _]}
  end

  test "dont display new hex version for too new elixir" do
    flush()
    bypass_csv([{"100.0.0", "100.0.0"}])

    Registry.open(registry_path: tmp_path(test_name() <> ".ets"))
    Registry.close
    refute_received {:mix_shell, :info, ["\e[33mA new Hex version is available" <> _]}
  end

  test "only check version once" do
    flush()
    bypass_csv([{"100.0.0", "1.0.0"}])

    Registry.open(registry_path: tmp_path(test_name() <> "1.ets"))
    Registry.close
    assert_received {:mix_shell, :info, ["\e[33mA new Hex version is available" <> _]}

    Registry.open(registry_path: tmp_path(test_name() <> "2.ets"))
    Registry.close
    refute_received {:mix_shell, :info, ["\e[33mA new Hex version is available" <> _]}
  end
end
