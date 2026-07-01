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

    test "rejects malformed durations" do
      assert :error == Cooldown.duration_to_seconds("")
      assert :error == Cooldown.duration_to_seconds("5")
      assert :error == Cooldown.duration_to_seconds("00")
      assert :error == Cooldown.duration_to_seconds("d")
      assert :error == Cooldown.duration_to_seconds("5m")
      assert :error == Cooldown.duration_to_seconds("5x")
      assert :error == Cooldown.duration_to_seconds("5dd")
      assert :error == Cooldown.duration_to_seconds("+5d")
      assert :error == Cooldown.duration_to_seconds("-5d")
      assert :error == Cooldown.duration_to_seconds("1_0d")
      assert :error == Cooldown.duration_to_seconds(" 5d")
      assert :error == Cooldown.duration_to_seconds("5d ")
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

  describe "strictest/1" do
    test "picks the candidate with the longest duration" do
      assert {{"myorg", "p"}, "14d"} ==
               Cooldown.strictest([
                 {:local, "7d"},
                 {{"myorg", "p"}, "14d"},
                 {{"myorg", "q"}, "3d"}
               ])
    end

    test "treats nil and empty as zero" do
      assert {:local, "7d"} ==
               Cooldown.strictest([
                 {:local, "7d"},
                 {{"myorg", "p"}, nil},
                 {{"myorg", "q"}, ""}
               ])
    end

    test "uses policy when local is 0d" do
      assert {{"myorg", "p"}, "14d"} ==
               Cooldown.strictest([
                 {:local, "0d"},
                 {{"myorg", "p"}, "14d"}
               ])
    end

    test "returns 0d when nothing is set" do
      assert {:local, "0d"} ==
               Cooldown.strictest([
                 {:local, "0d"},
                 {{"myorg", "p"}, nil}
               ])
    end

    test "returns :local when nothing else contributes" do
      assert {:local, "7d"} == Cooldown.strictest([{:local, "7d"}])
    end

    test "normalizes nil/empty local to 0d" do
      assert {{"myorg", "p"}, "14d"} ==
               Cooldown.strictest([
                 {:local, nil},
                 {{"myorg", "p"}, "14d"}
               ])

      assert {{"myorg", "p"}, "14d"} ==
               Cooldown.strictest([
                 {:local, ""},
                 {{"myorg", "p"}, "14d"}
               ])
    end

    test "normalizes unparseable and zero durations to 0d" do
      assert {:local, "0d"} == Cooldown.strictest([{:local, "garbage"}])
      assert {:local, "0d"} == Cooldown.strictest([{:local, "0"}])

      assert {{"myorg", "p"}, "14d"} ==
               Cooldown.strictest([
                 {:local, "5x"},
                 {{"myorg", "p"}, "14d"}
               ])
    end
  end

  describe "format_summary/2" do
    test "returns nil for an empty list" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      assert Cooldown.format_summary([], cutoff) == nil
    end

    test "returns nil when cutoff is :disabled" do
      now = System.system_time(:second)
      assert Cooldown.format_summary([{"hexpm", "foo", "1.0.0", now}], :disabled) == nil
    end

    test "renders a single entry with days ago and eligible date" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      now = System.system_time(:second)
      published_at = now - 3 * 86_400

      summary = Cooldown.format_summary([{"hexpm", "castore", "1.0.19", published_at}], cutoff)

      assert summary =~ "Versions filtered by cooldown:"
      assert summary =~ "castore 1.0.19"
      assert summary =~ "eligible #{Cooldown.eligible_on(published_at, cutoff)}"
    end

    test "groups by package, lists versions newest-first, dedupes identical entries" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      now = System.system_time(:second)
      young = now - 1 * 86_400

      entries = [
        {"hexpm", "plug", "1.16.5", young},
        {"hexpm", "castore", "1.0.19", young},
        {"hexpm", "castore", "1.0.19", young},
        {"hexpm", "castore", "1.0.18", young}
      ]

      summary = Cooldown.format_summary(entries, cutoff)

      # Packages alphabetically, versions newest-first; dedup keeps one castore 1.0.19.
      [_header, line1, line2, line3 | _] = String.split(summary, "\n", trim: true)
      assert line1 =~ "castore 1.0.19"
      assert line2 =~ "castore 1.0.18"
      assert line3 =~ "plug 1.16.5"

      occurrences = summary |> String.split("castore 1.0.19") |> length()
      assert occurrences == 2
    end

    test "caps the listing per package with an overflow line" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      young = System.system_time(:second) - 1 * 86_400

      entries = for minor <- 0..7, do: {"hexpm", "castore", "1.#{minor}.0", young}

      summary = Cooldown.format_summary(entries, cutoff)

      assert summary =~ "castore 1.7.0"
      assert summary =~ "castore 1.3.0"
      refute summary =~ "castore 1.2.0"
      assert summary =~ "  ...and 3 more"
    end

    test "omits entries with nil published_at" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      now = System.system_time(:second)

      entries = [
        {"hexpm", "legacy", "1.0.0", nil},
        {"hexpm", "castore", "1.0.19", now - 1 * 86_400}
      ]

      summary = Cooldown.format_summary(entries, cutoff)

      assert summary =~ "castore 1.0.19"
      refute summary =~ "legacy"
    end

    test "returns nil when every entry has nil published_at" do
      Hex.State.put(:cooldown, "7d")
      cutoff = Cooldown.build_cutoff()
      assert Cooldown.format_summary([{"hexpm", "legacy", "1.0.0", nil}], cutoff) == nil
    end
  end
end
