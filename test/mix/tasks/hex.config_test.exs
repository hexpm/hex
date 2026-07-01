defmodule Mix.Tasks.Hex.ConfigTest do
  use HexTest.Case

  test "config" do
    Process.put(:hex_test_app_name, :config_custom_api_url)
    Mix.Project.push(ReleaseCustomApiUrl.MixProject)

    in_tmp(fn ->
      System.put_env("HEX_HOME", File.cwd!())
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run([])

      assert_received {:mix_shell, :info, ["api_url: \"https://custom\" (using `mix.exs`)"]}
      assert_received {:mix_shell, :info, ["api_key: nil (default)"]}
      assert_received {:mix_shell, :info, ["offline: false (default)"]}
      assert_received {:mix_shell, :info, ["unsafe_https: false (default)"]}
      assert_received {:mix_shell, :info, ["unsafe_registry: false (default)"]}
      assert_received {:mix_shell, :info, ["http_proxy: nil (default)"]}
      assert_received {:mix_shell, :info, ["https_proxy: nil (default)"]}
      assert_received {:mix_shell, :info, ["no_proxy: nil (default)"]}
      assert_received {:mix_shell, :info, ["http_concurrency: 8 (default)"]}
      assert_received {:mix_shell, :info, ["http_timeout: nil (default)"]}
      assert_received {:mix_shell, :info, ["mirror_url: nil (default)"]}
      assert_received {:mix_shell, :info, ["trusted_mirror_url: nil (default)"]}
      assert_received {:mix_shell, :info, ["config_home:" <> _]}
      assert_received {:mix_shell, :info, ["no_short_urls: false (default)"]}
      assert_received {:mix_shell, :info, ["cooldown: \"0d\" (default)"]}
    end)
  after
    purge([ReleaseCustomApiUrl.MixProject])
  end

  test "cooldown config key" do
    in_tmp(fn ->
      System.put_env("HEX_HOME", File.cwd!())
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run(["cooldown"])
      assert_received {:mix_shell, :info, ["\"0d\""]}

      System.put_env("HEX_COOLDOWN", "7d")
      Hex.State.refresh()
      Mix.Tasks.Hex.Config.run(["cooldown"])
      assert_received {:mix_shell, :info, ["\"7d\""]}

      System.delete_env("HEX_COOLDOWN")
      Hex.State.refresh()
    end)
  end

  test "ignore_advisories and ignore_retirements config keys" do
    in_tmp(fn ->
      System.put_env("HEX_HOME", File.cwd!())
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run(["ignore_advisories"])
      assert_received {:mix_shell, :info, ["[]"]}

      Mix.Tasks.Hex.Config.run(["ignore_retirements"])
      assert_received {:mix_shell, :info, ["[]"]}

      System.put_env("HEX_IGNORE_ADVISORIES", "CVE-2026-32686, GHSA-xxxx-yyyy-zzzz")
      System.put_env("HEX_IGNORE_RETIREMENTS", "decimal,phoenix@1.0.0")
      Hex.State.refresh()

      assert Hex.State.fetch!(:ignore_advisories) == ["CVE-2026-32686", "GHSA-xxxx-yyyy-zzzz"]
      assert Hex.State.fetch!(:ignore_retirements) == [{"decimal", nil}, {"phoenix", "1.0.0"}]

      System.delete_env("HEX_IGNORE_ADVISORIES")
      System.delete_env("HEX_IGNORE_RETIREMENTS")
      Hex.State.refresh()
    end)
  end

  test "config key" do
    in_tmp(fn ->
      System.put_env("HEX_HOME", File.cwd!())
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run(["offline", "--delete"])

      Mix.Tasks.Hex.Config.run(["offline"])
      assert_received {:mix_shell, :info, ["false"]}

      System.put_env("HEX_OFFLINE", "true")
      Hex.State.refresh()
      Mix.Tasks.Hex.Config.run(["offline"])
      assert_received {:mix_shell, :info, ["true"]}

      System.delete_env("HEX_OFFLINE")
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run(["offline"])
      assert_received {:mix_shell, :info, ["false"]}

      assert_raise Mix.Error, "Invalid key foo", fn ->
        Mix.Tasks.Hex.Config.run(["foo", "bar"])
      end
    end)
  end

  test "api_key" do
    in_tmp(fn ->
      System.put_env("HEX_HOME", File.cwd!())
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run([])
      assert_received {:mix_shell, :info, ["api_key: nil (default)"]}

      Mix.Tasks.Hex.Config.run(["api_key", "foo"])
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run([])
      assert_received {:mix_shell, :info, ["api_key: \"foo\" (using " <> _]}

      Mix.Tasks.Hex.Config.run(["api_key", "--delete"])
      Hex.State.refresh()

      Mix.Tasks.Hex.Config.run([])
      assert_received {:mix_shell, :info, ["api_key: nil (default)"]}
    end)
  end

  test "direct api" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())
      assert Hex.Config.read() == []

      Hex.Config.update(key: "value")
      assert Hex.Config.read() == [key: "value"]

      Hex.Config.update(key: "other", foo: :bar)
      assert Hex.Config.read() == [key: "other", foo: :bar]
    end)
  end
end
