defmodule Hex.AuditTest do
  use HexTest.Case
  alias Hex.Registry.Server, as: Registry

  @lock %{mint: {:hex, :mint, "1.10.1", nil, [:mix], [], "hexpm", nil}}

  @advisory %{
    id: "EEF-CVE-2026-91043",
    summary: "Unbounded cookie headers exhaust client memory",
    html_url: "https://osv.dev/vulnerability/EEF-CVE-2026-91043",
    severity: :SEVERITY_HIGH
  }

  @policy %{
    repository: "myorg",
    name: "strict-prod",
    visibility: :VISIBILITY_PUBLIC,
    repositories: [
      %{
        repository: "hexpm",
        restriction: %{advisory_min_severity: :SEVERITY_HIGH},
        overrides: []
      }
    ]
  }

  defmodule LockedMint.MixProject do
    def project do
      [app: :locked_mint, version: "0.1.0", deps: []]
    end
  end

  setup do
    bypass = Bypass.open()
    Bypass.down(bypass)
    repos = Hex.State.fetch!(:repos)
    Hex.State.put(:repos, put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}"))
    Hex.State.put(:shell_process, self())
    # `mix hex.audit` persists the registry, so each test starts without a cache.
    registry_path = tmp_path("audit_cache.ets")
    File.rm(registry_path)
    Registry.open(check_version: false, registry_path: registry_path)
    :ok
  end

  test "fails when a locked package can't be fetched and isn't cached" do
    Registry.prefetch([{"hexpm", "mint"}])

    assert_raise Mix.Error, ~r"Could not audit mint 1.10.1,", fn ->
      Hex.Audit.run(@lock, nil, :default)
    end
  end

  test "fails when the cached registry entry doesn't include the locked version" do
    :sys.replace_state(Registry, fn %{ets: tid} = state ->
      :ets.insert(tid, {{:versions, "hexpm", "mint"}, ["1.9.0"]})
      state
    end)

    Registry.prefetch([{"hexpm", "mint"}])

    assert_raise Mix.Error, ~r"Could not audit mint 1.10.1,", fn ->
      Hex.Audit.run(@lock, nil, :default)
    end
  end

  test "mix hex.audit fails when mint 1.10.1 can't be fetched, even with a cached entry" do
    cache_mint_advisory()

    assert_raise Mix.Error, ~r"Could not audit mint 1.10.1, .*HEX_OFFLINE=1", fn ->
      audit_locked_mint()
    end
  end

  test "mix hex.audit with HEX_OFFLINE=1 reports the cached advisory of mint 1.10.1" do
    Hex.State.put(:offline, true)
    cache_mint_advisory()

    assert catch_throw(audit_locked_mint()) == {:exit_code, 1}
    assert shell_output() =~ "EEF-CVE-2026-91043"
  end

  test "the deps.get policy check rejects mint 1.10.1 from the cache when the fetch fails" do
    cache_mint_advisory()
    Registry.prefetch([{"hexpm", "mint"}])

    assert [%{package: "mint", version: "1.10.1", detail: %{id: "EEF-CVE-2026-91043"}}] =
             Hex.Audit.run(@lock, @policy, :policy).advisories
  end

  defp cache_mint_advisory() do
    :sys.replace_state(Registry, fn %{ets: tid} = state ->
      :ets.insert(tid, {{:versions, "hexpm", "mint"}, ["1.10.1"]})
      :ets.insert(tid, {{:advisories, "hexpm", "mint", "1.10.1"}, [@advisory]})
      state
    end)
  end

  defp audit_locked_mint() do
    Mix.Project.push(LockedMint.MixProject)

    in_tmp(fn ->
      Mix.Dep.Lock.write(@lock)
      Mix.Task.run("hex.audit")
    end)
  end
end
