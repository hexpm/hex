defmodule Hex.Registry.ServerPolicyTest do
  use HexTest.Case, async: false
  alias Hex.Registry.Server, as: Registry

  setup do
    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)

    repos =
      Map.put(repos, "hexpm:myorg", %{
        url: "http://localhost:#{bypass.port}",
        public_key: File.read!(fixture_path("test_pub.pem")),
        auth_key: "key",
        trusted: true,
        oauth_exchange: false,
        repo_organization: "myorg"
      })

    Hex.State.put(:repos, repos)
    {:ok, bypass: bypass}
  end

  defp signed_policy(fields) do
    private_key = File.read!(fixture_path("test_priv.pem"))
    payload = :mix_hex_registry.encode_policy(Map.new(fields))
    signed = :mix_hex_registry.sign_protobuf(payload, private_key)
    :zlib.gzip(signed)
  end

  defp fresh_policy(repositories) do
    base = %{
      repository: "myorg",
      name: "strict-prod",
      visibility: :VISIBILITY_PUBLIC,
      repositories: repositories
    }

    signed_policy(base)
  end

  test "prefetch_policies/1 fetches and decodes; policy/2 returns the decoded map",
       %{bypass: bypass} do
    repositories = [
      %{
        repository: "hexpm",
        restriction: %{advisory_min_severity: :SEVERITY_HIGH},
        overrides: []
      }
    ]

    Bypass.expect_once(bypass, "GET", "/repos/myorg/policies/strict-prod", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("etag", "\"v1\"")
      |> Plug.Conn.resp(200, fresh_policy(repositories))
    end)

    in_tmp("registry_policy_fetch", fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Registry.open(check_version: false, registry_path: Path.join(File.cwd!(), "cache.ets"))

      assert :ok = Registry.prefetch_policies([{"hexpm:myorg", "strict-prod"}])
      assert {:ok, policy} = Registry.policy("hexpm:myorg", "strict-prod")
      assert policy.name == "strict-prod"
      assert [%{repository: "hexpm", restriction: restriction}] = policy.repositories
      assert restriction.advisory_min_severity == :SEVERITY_HIGH
    end)
  end

  test "policy/2 falls back to the cached payload when the registry is unreachable",
       %{bypass: bypass} do
    Bypass.expect_once(bypass, "GET", "/repos/myorg/policies/strict-prod", fn conn ->
      Plug.Conn.resp(conn, 200, fresh_policy([]))
    end)

    in_tmp("registry_policy_cache_fallback", fn ->
      Hex.State.put(:cache_home, File.cwd!())
      registry_path = Path.join(File.cwd!(), "cache.ets")
      Registry.open(check_version: false, registry_path: registry_path)

      assert :ok = Registry.prefetch_policies([{"hexpm:myorg", "strict-prod"}])
      assert {:ok, _} = Registry.policy("hexpm:myorg", "strict-prod")

      Registry.persist()
      Registry.close()
      Bypass.down(bypass)

      Registry.open(check_version: false, registry_path: registry_path)
      assert :ok = Registry.prefetch_policies([{"hexpm:myorg", "strict-prod"}])
      assert {:ok, policy} = Registry.policy("hexpm:myorg", "strict-prod")
      assert policy.name == "strict-prod"
    end)
  end

  test "close/0 waits for in-flight policy fetches", %{bypass: bypass} do
    Bypass.expect_once(bypass, "GET", "/repos/myorg/policies/strict-prod", fn conn ->
      Process.sleep(100)
      Plug.Conn.resp(conn, 200, fresh_policy([]))
    end)

    in_tmp("registry_policy_close_in_flight", fn ->
      Hex.State.put(:cache_home, File.cwd!())
      registry_path = Path.join(File.cwd!(), "cache.ets")
      Registry.open(check_version: false, registry_path: registry_path)

      pid = Process.whereis(Registry)
      assert :ok = Registry.prefetch_policies([{"hexpm:myorg", "strict-prod"}])
      assert :ok = Registry.close()

      # Without waiting, the fetch result would land in a deleted ETS table
      # and crash the server.
      Process.sleep(150)
      assert Process.whereis(Registry) == pid
    end)
  end

  test "prefetch_policies/1 raises a helpful error in offline mode when missing" do
    in_tmp("registry_policy_offline", fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Hex.State.put(:offline, true)
      Registry.open(check_version: false, registry_path: Path.join(File.cwd!(), "cache.ets"))

      assert_raise Mix.Error,
                   ~r"Hex is running in offline mode and policy hexpm:myorg/strict-prod is not cached locally",
                   fn ->
                     Registry.prefetch_policies([{"hexpm:myorg", "strict-prod"}])
                   end
    end)
  end
end
