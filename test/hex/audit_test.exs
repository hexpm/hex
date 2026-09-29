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

  setup do
    bypass = Bypass.open()
    Bypass.down(bypass)
    repos = Hex.State.fetch!(:repos)
    Hex.State.put(:repos, put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}"))
    Hex.State.put(:shell_process, self())
    Registry.open(check_version: false, registry_path: tmp_path("audit_cache.ets"))
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

  test "uses the cached registry entry when a locked package can't be fetched" do
    :sys.replace_state(Registry, fn %{ets: tid} = state ->
      :ets.insert(tid, {{:versions, "hexpm", "mint"}, ["1.10.1"]})
      :ets.insert(tid, {{:advisories, "hexpm", "mint", "1.10.1"}, [@advisory]})
      state
    end)

    Registry.prefetch([{"hexpm", "mint"}])

    assert [%{package: "mint", advisories: [@advisory]}] =
             Hex.Audit.run(@lock, nil, :default).raw_advisories
  end
end
