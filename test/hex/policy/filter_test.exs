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

    test "blocks unknown severities instead of letting them slip under the threshold" do
      p = policy(tab(restriction: %{advisory_min_severity: :SEVERITY_HIGH}))

      c = candidate(advisories: [%{severity: 99}])
      assert {:blocked, reasons} = Filter.classify(p, c)
      assert {:advisory, :SEVERITY_HIGH} in reasons

      c = candidate(advisories: [%{}])
      assert {:blocked, _reasons} = Filter.classify(p, c)
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

  describe "classify/3 - scoped overrides" do
    @advisory %{
      id: "GHSA-test-0001",
      aliases: ["CVE-2026-12345"],
      severity: :SEVERITY_HIGH
    }

    test "an advisory override matches primary IDs and aliases without regard to case" do
      for id <- ["GHSA-test-0001", "cve-2026-12345"] do
        p =
          policy(
            tab(
              restriction: %{advisory_min_severity: :SEVERITY_LOW},
              overrides: [
                %{
                  action: :OVERRIDE_ACTION_ADVISORY,
                  ref: %{package: "phoenix"},
                  advisory_id: id
                }
              ]
            )
          )

        assert :allowed == Filter.classify(p, candidate(advisories: [@advisory]))
      end
    end

    test "a requirement narrows the exception using Hex requirement semantics" do
      p =
        policy(
          tab(
            restriction: %{advisory_min_severity: :SEVERITY_LOW},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "phoenix", requirement: "~> 1.0.0"},
                advisory_id: "CVE-2026-12345"
              }
            ]
          )
        )

      assert :allowed == Filter.classify(p, candidate(version: "1.0.9", advisories: [@advisory]))

      assert {:blocked, [{:advisory, :SEVERITY_LOW}]} =
               Filter.classify(p, candidate(version: "1.1.0", advisories: [@advisory]))
    end

    test "an advisory override removes only the matching advisory" do
      other = %{id: "GHSA-test-0002", aliases: [], severity: :SEVERITY_CRITICAL}

      p =
        policy(
          tab(
            restriction: %{advisory_min_severity: :SEVERITY_LOW},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "phoenix"},
                advisory_id: "CVE-2026-12345"
              }
            ]
          )
        )

      assert {:blocked, [{:advisory, :SEVERITY_LOW}]} =
               Filter.classify(p, candidate(advisories: [@advisory, other]))
    end

    test "a new advisory for an overridden release remains blocked" do
      overridden = %{
        id: "GHSA-decimal-4242",
        aliases: ["CVE-2026-4242"],
        severity: :SEVERITY_HIGH
      }

      newly_published = %{
        id: "GHSA-decimal-4243",
        aliases: ["CVE-2026-4243"],
        severity: :SEVERITY_CRITICAL
      }

      p =
        policy(
          tab(
            restriction: %{advisory_min_severity: :SEVERITY_LOW},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "decimal", requirement: "== 1.0.0"},
                advisory_id: "CVE-2026-4242"
              }
            ]
          )
        )

      assert {:blocked, [{:advisory, :SEVERITY_LOW}]} =
               Filter.classify(
                 p,
                 candidate(
                   package: "decimal",
                   version: "1.0.0",
                   advisories: [overridden, newly_published]
                 )
               )
    end

    test "a retirement override removes only its selected reason" do
      p =
        policy(
          tab(
            restriction: %{retirement_reasons: [:RETIRED_SECURITY]},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_RETIREMENT,
                ref: %{package: "phoenix"},
                retirement_reason: :RETIRED_SECURITY
              }
            ]
          )
        )

      assert :allowed == Filter.classify(p, candidate(retired: %{reason: :RETIRED_SECURITY}))
    end

    test "a changed retirement reason is not accepted by an existing override" do
      p =
        policy(
          tab(
            restriction: %{
              retirement_reasons: [:RETIRED_DEPRECATED, :RETIRED_SECURITY]
            },
            overrides: [
              %{
                action: :OVERRIDE_ACTION_RETIREMENT,
                ref: %{package: "phoenix"},
                retirement_reason: :RETIRED_DEPRECATED
              }
            ]
          )
        )

      assert {:blocked, [{:retirement, :RETIRED_SECURITY}]} =
               Filter.classify(p, candidate(retired: %{reason: :RETIRED_SECURITY}))
    end

    test "an advisory override never bypasses cooldowns" do
      now = 1_700_000_000

      p =
        policy(
          tab(
            restriction: %{advisory_min_severity: :SEVERITY_LOW, cooldown: "14d"},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "phoenix"},
                advisory_id: "CVE-2026-12345"
              }
            ]
          )
        )

      assert {:blocked, [{:cooldown, "14d", %Date{}}]} =
               Filter.classify(
                 p,
                 candidate(advisories: [@advisory], published_at: now - 86_400),
                 now: now
               )
    end

    test "a cooldown override bypasses only the policy cooldown" do
      now = 1_700_000_000

      p =
        policy(
          tab(
            restriction: %{advisory_min_severity: :SEVERITY_LOW, cooldown: "14d"},
            overrides: [
              %{action: :OVERRIDE_ACTION_COOLDOWN, ref: %{package: "phoenix"}}
            ]
          )
        )

      assert {:blocked, [{:advisory, :SEVERITY_LOW}]} =
               Filter.classify(
                 p,
                 candidate(advisories: [@advisory], published_at: now - 86_400),
                 now: now
               )
    end

    test "malformed and unknown overrides are ignored" do
      invalid = [
        %{advisory_id: "CVE-2026-12345"},
        %{
          action: :OVERRIDE_ACTION_ADVISORY,
          ref: %{package: "phoenix"},
          advisory_id: "CVE-2026-12345",
          retirement_reason: :RETIRED_SECURITY
        },
        %{
          action: :OVERRIDE_ACTION_RETIREMENT,
          ref: %{package: "phoenix"},
          retirement_reason: 99
        },
        %{
          action: :OVERRIDE_ACTION_ADVISORY,
          ref: %{package: "phoenix", requirement: "not a requirement"},
          advisory_id: "CVE-2026-12345"
        },
        %{
          action: :OVERRIDE_ACTION_ADVISORY,
          ref: %{package: "phoenix"},
          advisory_id: ""
        },
        %{action: 99, ref: %{package: "phoenix"}}
      ]

      for override <- invalid do
        p =
          policy(
            tab(
              restriction: %{advisory_min_severity: :SEVERITY_LOW},
              overrides: [override]
            )
          )

        assert {:blocked, [{:advisory, :SEVERITY_LOW}]} =
                 Filter.classify(p, candidate(advisories: [@advisory]))
      end
    end

    test "comment limits count Unicode codepoints" do
      override = %{
        action: :OVERRIDE_ACTION_ALLOW,
        ref: %{package: "phoenix"},
        comment: String.duplicate("e\u0301", 250)
      }

      assert Filter.valid_override?(override)
      refute Filter.valid_override?(%{override | comment: String.duplicate("e\u0301", 251)})
    end

    test "invalid UTF-8 strings decoded from the protocol fail closed" do
      for {invalid_field, invalid_value} <- [
            {:package, <<255>>},
            {:package, "phoenix\nspoof"},
            {:package, "phoenix\u202Espoof"},
            {:advisory_id, <<255>>},
            {:advisory_id, "CVE-2026-12345\nspoof"},
            {:requirement, <<255>>},
            {:comment, <<255>>},
            {:comment, "line one\nline two"},
            {:comment, "line\u2028separator"},
            {:comment, "paragraph\u2029separator"},
            {:comment, "bidi\u202Eoverride"}
          ] do
        encoded_value = if String.valid?(invalid_value), do: invalid_value, else: "!"
        ref = %{package: "phoenix"}

        override = %{
          action: :OVERRIDE_ACTION_ADVISORY,
          ref: ref,
          advisory_id: "CVE-2026-12345"
        }

        override =
          case invalid_field do
            :package -> put_in(override, [:ref, :package], encoded_value)
            :advisory_id -> Map.put(override, :advisory_id, encoded_value)
            :requirement -> put_in(override, [:ref, :requirement], encoded_value)
            :comment -> Map.put(override, :comment, encoded_value)
          end

        encoded =
          :mix_hex_registry.encode_policy(%{
            repository: "myorg",
            name: "strict",
            visibility: :VISIBILITY_PUBLIC,
            repositories: [
              %{
                repository: "hexpm",
                restriction: %{advisory_min_severity: :SEVERITY_LOW},
                overrides: [override]
              }
            ]
          })

        encoded =
          if encoded_value == invalid_value do
            encoded
          else
            :binary.replace(encoded, encoded_value, invalid_value)
          end

        assert {:ok, decoded} = :mix_hex_registry.decode_policy(encoded, :no_verify, :no_verify)

        assert {:blocked, [{:advisory, :SEVERITY_LOW}]} =
                 Filter.classify(decoded, candidate(advisories: [@advisory]))
      end
    end

    test "explain includes an optional comment" do
      p =
        policy(
          tab(
            restriction: %{advisory_min_severity: :SEVERITY_LOW},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "phoenix"},
                advisory_id: "CVE-2026-12345",
                comment: "The vulnerable code path is disabled"
              }
            ]
          )
        )

      assert {:allowed, [acceptance]} = Filter.explain(p, candidate(advisories: [@advisory]))
      assert acceptance.source == :override
      assert acceptance.identifier == "CVE-2026-12345"
      assert Filter.acceptance_message(acceptance) == "The vulnerable code path is disabled"

      assert Filter.acceptance_message(%{acceptance | comment: nil}) ==
               "Accepted by the active dependency policy."
    end
  end

  describe "audit_finding/4" do
    test "overrides mode applies ALLOW and scoped overrides but not thresholds" do
      low = %{id: "CVE-low", aliases: [], severity: :SEVERITY_LOW}

      threshold_policy =
        policy(tab(restriction: %{advisory_min_severity: :SEVERITY_HIGH}))

      assert :active =
               Filter.audit_finding(threshold_policy, candidate(), {:advisory, low}, :overrides)

      assert {:accepted, %{source: :policy}} =
               Filter.audit_finding(threshold_policy, candidate(), {:advisory, low}, :policy)

      allow_policy =
        policy(tab(overrides: [%{action: :OVERRIDE_ACTION_ALLOW, ref: %{package: "phoenix"}}]))

      assert {:accepted, %{source: :override}} =
               Filter.audit_finding(allow_policy, candidate(), {:advisory, low}, :overrides)
    end

    test "policy mode keeps findings rejected by DENY and restriction rules" do
      advisory = %{id: "CVE-high", aliases: [], severity: :SEVERITY_HIGH}

      deny_policy =
        policy(tab(overrides: [%{action: :OVERRIDE_ACTION_DENY, ref: %{package: "phoenix"}}]))

      assert :active =
               Filter.audit_finding(deny_policy, candidate(), {:advisory, advisory}, :policy)

      restricted = policy(tab(restriction: %{advisory_min_severity: :SEVERITY_HIGH}))

      assert :active =
               Filter.audit_finding(restricted, candidate(), {:advisory, advisory}, :policy)
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
