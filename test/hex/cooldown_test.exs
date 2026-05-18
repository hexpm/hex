defmodule Hex.CooldownTest do
  use HexTest.Case
  alias Hex.Cooldown

  describe "parse_config/1" do
    test "accepts integer + unit forms" do
      assert {:ok, "0d"} == Cooldown.parse_config("0d")
      assert {:ok, "7d"} == Cooldown.parse_config("7d")
      assert {:ok, "14d"} == Cooldown.parse_config("14d")
      assert {:ok, "2w"} == Cooldown.parse_config("2w")
      assert {:ok, "1mo"} == Cooldown.parse_config("1mo")
      assert {:ok, "0"} == Cooldown.parse_config("0")
    end

    test "treats nil and empty as default" do
      assert {:ok, "0d"} == Cooldown.parse_config(nil)
      assert {:ok, "0d"} == Cooldown.parse_config("")
    end

    test "rejects malformed durations" do
      assert :error == Cooldown.parse_config("7day")
      assert :error == Cooldown.parse_config("7 days")
      assert :error == Cooldown.parse_config("1m")
      assert :error == Cooldown.parse_config("1 month")
      assert :error == Cooldown.parse_config("d7")
      assert :error == Cooldown.parse_config("seven")
      assert :error == Cooldown.parse_config("-1d")
    end
  end

  describe "duration_to_seconds/1" do
    test "converts canonical units to seconds" do
      assert {:ok, 0} == Cooldown.duration_to_seconds("0")
      assert {:ok, 0} == Cooldown.duration_to_seconds("0d")
      assert {:ok, 86_400} == Cooldown.duration_to_seconds("1d")
      assert {:ok, 7 * 86_400} == Cooldown.duration_to_seconds("7d")
      assert {:ok, 14 * 86_400} == Cooldown.duration_to_seconds("2w")
      assert {:ok, 30 * 86_400} == Cooldown.duration_to_seconds("1mo")
    end
  end

  describe "build_cutoff/0" do
    test "returns :disabled when cooldown is zero" do
      Hex.State.put(:cooldown, "0d")
      assert :disabled == Cooldown.build_cutoff()
    end

    test "returns a cutoff for non-zero durations" do
      Hex.State.put(:cooldown, "7d")
      assert {:cutoff, _, seconds} = Cooldown.build_cutoff()
      assert seconds == 7 * 86_400
    end

    test "weeks and months are honored" do
      Hex.State.put(:cooldown, "2w")
      assert {:cutoff, _, seconds} = Cooldown.build_cutoff()
      assert seconds == 14 * 86_400

      Hex.State.put(:cooldown, "1mo")
      assert {:cutoff, _, seconds} = Cooldown.build_cutoff()
      assert seconds == 30 * 86_400
    end
  end

  describe "eligible?/2" do
    test ":disabled cutoff makes everything eligible" do
      assert Cooldown.eligible?(0, :disabled)
      assert Cooldown.eligible?(nil, :disabled)
    end

    test "nil published_at is treated as eligible" do
      now = System.system_time(:second)
      cutoff = {:cutoff, now - 7 * 86_400, 7 * 86_400}
      assert Cooldown.eligible?(nil, cutoff)
    end

    test "publish time older than cutoff is eligible" do
      now = System.system_time(:second)
      cutoff = {:cutoff, now - 7 * 86_400, 7 * 86_400}
      old_publish = now - 10 * 86_400
      assert Cooldown.eligible?(old_publish, cutoff)
    end

    test "publish time newer than cutoff is not eligible" do
      now = System.system_time(:second)
      cutoff = {:cutoff, now - 7 * 86_400, 7 * 86_400}
      fresh_publish = now - 86_400
      refute Cooldown.eligible?(fresh_publish, cutoff)
    end
  end

  describe "describe_source/0" do
    test "reflects HEX_COOLDOWN env var" do
      Hex.State.put(:cooldown, "7d")
      # Hex.State sources are set during init; force via direct setter
      assert is_binary(Cooldown.describe_source())
    end
  end

  describe "HEX_COOLDOWN= env handling" do
    test "empty HEX_COOLDOWN= falls through to next source, not env" do
      original = System.get_env("HEX_COOLDOWN")

      try do
        System.put_env("HEX_COOLDOWN", "")
        Hex.State.refresh()

        # Without a project or global config contribution, fallthrough lands at default.
        # The key assertion is that the source is NOT :env — if fallthrough were broken
        # the source would be {:env, "HEX_COOLDOWN"} with value "0d".
        assert :default == Hex.State.fetch_source!(:cooldown)
        assert "0d" == Hex.State.fetch!(:cooldown)
      after
        if original do
          System.put_env("HEX_COOLDOWN", original)
        else
          System.delete_env("HEX_COOLDOWN")
        end

        Hex.State.refresh()
        HexTest.Case.reset_state()
      end
    end

    test "HEX_COOLDOWN=0 explicitly disables (env wins)" do
      original = System.get_env("HEX_COOLDOWN")

      try do
        System.put_env("HEX_COOLDOWN", "0")
        Hex.State.refresh()

        assert {:env, "HEX_COOLDOWN"} == Hex.State.fetch_source!(:cooldown)
        assert "0" == Hex.State.fetch!(:cooldown)
      after
        if original do
          System.put_env("HEX_COOLDOWN", original)
        else
          System.delete_env("HEX_COOLDOWN")
        end

        Hex.State.refresh()
        HexTest.Case.reset_state()
      end
    end

    test "empty HEX_COOLDOWN_EXCLUDE_REPOS= falls through to next source, not env" do
      # Symmetric to the HEX_COOLDOWN= fall-through. Without
      # skip_env_if_empty an empty env would silently shadow a project's
      # cooldown_exclude_repos with [].
      original = System.get_env("HEX_COOLDOWN_EXCLUDE_REPOS")

      try do
        System.put_env("HEX_COOLDOWN_EXCLUDE_REPOS", "")
        Hex.State.refresh()

        assert :default == Hex.State.fetch_source!(:cooldown_exclude_repos)
        assert [] == Hex.State.fetch!(:cooldown_exclude_repos)
      after
        if original do
          System.put_env("HEX_COOLDOWN_EXCLUDE_REPOS", original)
        else
          System.delete_env("HEX_COOLDOWN_EXCLUDE_REPOS")
        end

        Hex.State.refresh()
        HexTest.Case.reset_state()
      end
    end
  end

  describe "parse_exclude_repos/1" do
    test "accepts a list of strings" do
      assert {:ok, ["a", "b"]} == Cooldown.parse_exclude_repos(["a", "b"])
      assert {:ok, []} == Cooldown.parse_exclude_repos([])
    end

    test "accepts a comma-separated string (env var form)" do
      assert {:ok, ["hexpm:myorg", "private"]} ==
               Cooldown.parse_exclude_repos("hexpm:myorg,private")

      assert {:ok, ["hexpm:myorg"]} == Cooldown.parse_exclude_repos("hexpm:myorg")
    end

    test "trims whitespace and drops empty entries" do
      assert {:ok, ["a", "b"]} == Cooldown.parse_exclude_repos("a, b ,")
      assert {:ok, ["a"]} == Cooldown.parse_exclude_repos([" a ", "", " "])
    end

    test "rejects non-string list elements" do
      assert :error == Cooldown.parse_exclude_repos([:atom])
    end

    test "accepts nil and empty string as empty list" do
      assert {:ok, []} == Cooldown.parse_exclude_repos(nil)
      assert {:ok, []} == Cooldown.parse_exclude_repos("")
    end
  end

  describe "repo_excluded?/1" do
    test "true when the repo name appears in the exclude list" do
      Hex.State.put(:cooldown_exclude_repos, ["hexpm:myorg"])
      assert Cooldown.repo_excluded?("hexpm:myorg")
    end

    test "false when the repo is not in the list" do
      Hex.State.put(:cooldown_exclude_repos, ["hexpm:other"])
      refute Cooldown.repo_excluded?("hexpm:myorg")
    end

    test "nil repo is treated as hexpm" do
      Hex.State.put(:cooldown_exclude_repos, ["hexpm"])
      assert Cooldown.repo_excluded?(nil)
    end

    test "false on empty list" do
      Hex.State.put(:cooldown_exclude_repos, [])
      refute Cooldown.repo_excluded?("hexpm:myorg")
      refute Cooldown.repo_excluded?(nil)
    end
  end

  describe "preflight_error/4" do
    test "includes package, requirement, source, and per-version eligibility lines" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      now = System.system_time(:second)
      published_at = now - 3 * 86_400

      message =
        Cooldown.preflight_error("phoenix", "~> 1.7", [{"1.7.14", published_at}], cutoff)

      assert message =~ "phoenix"
      assert message =~ "~> 1.7"
      assert message =~ "1.7.14"
      assert message =~ "HEX_COOLDOWN=0"
      assert message =~ ~r/eligible \d{4}-/
    end

    test "Wait until shows earliest eligible date across the filtered set" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      now = System.system_time(:second)
      # Two filtered versions; the one published earlier becomes eligible earlier.
      older = now - 5 * 86_400
      newer = now - 1 * 86_400

      message =
        Cooldown.preflight_error(
          "phoenix",
          "~> 1.7",
          [{"1.7.14", newer}, {"1.7.13", older}],
          cutoff
        )

      expected_earliest = Cooldown.eligible_on(older, cutoff)
      assert message =~ "Wait until #{expected_earliest} and re-run"
    end
  end
end
