defmodule Hex.Policy.FilterTest do
  use HexTest.Case
  alias Hex.Policy.Filter

  # A policy with a single repository tab.
  defp policy(tab, opts \\ []) do
    %{
      repository: Keyword.get(opts, :org, "myorg"),
      name: Keyword.get(opts, :name, "strict-prod"),
      visibility: :VISIBILITY_PUBLIC,
      repositories: [tab]
    }
  end

  defp tab(fields) do
    Map.merge(%{repository: "hexpm", overrides: []}, Map.new(fields))
  end

  defp candidate(fields \\ []) do
    Map.merge(
      %{
        repo: "hexpm",
        package: "phoenix",
        version: "1.0.0",
        advisories: [],
        retired: nil,
        published_at: nil
      },
      Map.new(fields)
    )
  end

  describe "classify/3 — advisory rule" do
    test "blocks when release advisory >= threshold" do
      p = policy(tab(restriction: %{advisory_min_severity: :SEVERITY_HIGH}))
      c = candidate(advisories: [%{severity: :SEVERITY_HIGH}])
      assert {:blocked, reasons} = Filter.classify(p, c)
      assert {:advisory, :SEVERITY_HIGH} in reasons
    end

    test "allows when release advisory < threshold" do
      p = policy(tab(restriction: %{advisory_min_severity: :SEVERITY_HIGH}))
      c = candidate(advisories: [%{severity: :SEVERITY_LOW}])
      assert :allowed == Filter.classify(p, c)
    end

    test "allows when no advisories" do
      p = policy(tab(restriction: %{advisory_min_severity: :SEVERITY_HIGH}))
      assert :allowed == Filter.classify(p, candidate())
    end

    test "allows when the tab has no restriction" do
      p = policy(tab(%{}))
      c = candidate(advisories: [%{severity: :SEVERITY_CRITICAL}])
      assert :allowed == Filter.classify(p, c)
    end
  end

  describe "classify/3 — retirement rule" do
    test "blocks when release retired with selected reason" do
      p =
        policy(tab(restriction: %{retirement_reasons: [:RETIRED_SECURITY, :RETIRED_DEPRECATED]}))

      c = candidate(retired: %{reason: :RETIRED_SECURITY})
      assert {:blocked, reasons} = Filter.classify(p, c)
      assert {:retirement, :RETIRED_SECURITY} in reasons
    end

    test "allows when reason not in set" do
      p = policy(tab(restriction: %{retirement_reasons: [:RETIRED_SECURITY]}))
      c = candidate(retired: %{reason: :RETIRED_RENAMED})
      assert :allowed == Filter.classify(p, c)
    end

    test "allows when not retired" do
      p = policy(tab(restriction: %{retirement_reasons: [:RETIRED_SECURITY]}))
      assert :allowed == Filter.classify(p, candidate())
    end
  end

  describe "classify/3 — cooldown rule" do
    @now 1_700_000_000

    test "blocks a release younger than the cooldown" do
      p = policy(tab(restriction: %{cooldown: "14d"}))
      c = candidate(published_at: @now - 1 * 86_400)
      assert {:blocked, [{:cooldown, "14d", %Date{}}]} = Filter.classify(p, c, now: @now)
    end

    test "allows a release older than the cooldown" do
      p = policy(tab(restriction: %{cooldown: "14d"}))
      c = candidate(published_at: @now - 20 * 86_400)
      assert :allowed == Filter.classify(p, c, now: @now)
    end

    test "allows when published_at is unknown" do
      p = policy(tab(restriction: %{cooldown: "14d"}))
      assert :allowed == Filter.classify(p, candidate(published_at: nil), now: @now)
    end

    test "a zero cooldown never blocks" do
      p = policy(tab(restriction: %{cooldown: "0"}))
      c = candidate(published_at: @now)
      assert :allowed == Filter.classify(p, c, now: @now)
    end
  end

  describe "classify/3 — overrides" do
    test "an allow override bypasses the restriction" do
      p =
        policy(
          tab(
            restriction: %{advisory_min_severity: :SEVERITY_LOW},
            overrides: [%{action: :OVERRIDE_ACTION_ALLOW, ref: %{package: "phoenix"}}]
          )
        )

      c = candidate(advisories: [%{severity: :SEVERITY_CRITICAL}])
      assert :allowed == Filter.classify(p, c)
    end

    test "a deny override blocks" do
      p = policy(tab(overrides: [%{action: :OVERRIDE_ACTION_DENY, ref: %{package: "phoenix"}}]))
      assert {:blocked, [:override_deny]} = Filter.classify(p, candidate())
    end

    test "most specific override wins (versioned allow beats bare deny)" do
      p =
        policy(
          tab(
            overrides: [
              %{action: :OVERRIDE_ACTION_DENY, ref: %{package: "phoenix"}},
              %{
                action: :OVERRIDE_ACTION_ALLOW,
                ref: %{package: "phoenix", requirement: "== 1.7.10"}
              }
            ]
          )
        )

      assert :allowed == Filter.classify(p, candidate(version: "1.7.10"))
      assert {:blocked, [:override_deny]} = Filter.classify(p, candidate(version: "1.7.11"))
    end

    test "an override for a different package does not match" do
      p = policy(tab(overrides: [%{action: :OVERRIDE_ACTION_DENY, ref: %{package: "ecto"}}]))
      assert :allowed == Filter.classify(p, candidate(package: "phoenix"))
    end
  end

  describe "classify/3 — repository matching" do
    test "a policy does not constrain a repository it has no tab for" do
      p = policy(tab(repository: "hexpm", restriction: %{advisory_min_severity: :SEVERITY_LOW}))
      c = candidate(repo: "hexpm:myorg", advisories: [%{severity: :SEVERITY_CRITICAL}])
      assert :allowed == Filter.classify(p, c)
    end

    test "the org tab matches a hexpm:<org> candidate repo" do
      p = policy(tab(repository: "myorg", restriction: %{advisory_min_severity: :SEVERITY_LOW}))
      c = candidate(repo: "hexpm:myorg", advisories: [%{severity: :SEVERITY_HIGH}])
      assert {:blocked, _} = Filter.classify(p, c)
    end
  end
end
