defmodule Mix.Tasks.Hex.AuditTest do
  use HexTest.IntegrationCase

  @package :test_package
  @package_name Atom.to_string(@package)

  @advisory %{
    id: "GHSA-test-0001",
    summary: "Remote code execution via crafted input",
    html_url: "https://github.com/advisories/GHSA-test-0001",
    severity: :SEVERITY_HIGH,
    api_url: "https://hex.pm/api/advisories/GHSA-test-0001"
  }

  defmodule RetiredDeps.MixProject do
    def project do
      [app: :test_app, version: "0.0.1", deps: [{:test_package, ">= 0.1.0"}]]
    end
  end

  setup_all do
    auth = Hexpm.new_user("audit_user", "audit@mail.com", "passpass", "key")
    {:ok, [auth: auth]}
  end

  test "audit (retired package without a message)", context do
    with_test_package("0.1.0", context, fn ->
      retire_test_package("0.1.0", "security")

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}
      assert_retired_section(@package_name, "0.1.0", "(security)")
      assert_received {:mix_shell, :error, ["Found retired packages"]}
    end)
  end

  test "audit (retired package with a custom message)", context do
    with_test_package("0.2.0", context, fn ->
      retire_test_package("0.2.0", "invalid", "Superseded by v1.0.0")

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}
      assert_retired_section(@package_name, "0.2.0", "(invalid) Superseded by v1.0.0")
      assert_received {:mix_shell, :error, ["Found retired packages"]}
    end)
  end

  test "audit (no retired packages)", context do
    with_test_package("1.0.0", context, fn ->
      Mix.Task.run("hex.audit")
      assert_received {:mix_shell, :info, ["No retired or security advisory packages found"]}
    end)
  end

  test "audit (package with a security advisory)", context do
    with_test_package("1.1.0", context, fn ->
      inject_advisory(@package_name, "1.1.0", [@advisory])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      assert_advisory_section(@package_name, "1.1.0", @advisory)
      assert_received {:mix_shell, :error, ["Found packages with security advisories"]}
      refute_received {:mix_shell, :error, ["Found retired packages"]}
    end)
  end

  test "audit (package with both retirement and security advisory)", context do
    with_test_package("1.2.0", context, fn ->
      :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
        :ets.insert(
          tid,
          {{:retired, "hexpm", @package_name, "1.2.0"}, %{reason: :RETIRED_SECURITY}}
        )

        :ets.insert(tid, {{:advisories, "hexpm", @package_name, "1.2.0"}, [@advisory]})
        state
      end)

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      assert_retired_section(@package_name, "1.2.0", "(security)")
      assert_advisory_section(@package_name, "1.2.0", @advisory)
      assert_received {:mix_shell, :error, ["Found retired packages"]}
      assert_received {:mix_shell, :error, ["Found packages with security advisories"]}
    end)
  end

  test "audit (package with multiple advisories)", context do
    other_advisory = %{
      id: "GHSA-test-0002",
      summary: "Another vulnerability",
      html_url: "https://github.com/advisories/GHSA-test-0002",
      severity: :SEVERITY_MEDIUM
    }

    with_test_package("1.3.0", context, fn ->
      inject_advisory(@package_name, "1.3.0", [@advisory, other_advisory])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      assert_advisory_section(@package_name, "1.3.0", @advisory)
      assert_advisory_section(@package_name, "1.3.0", other_advisory)
    end)
  end

  test "audit (advisories sharing a CVE alias are deduplicated)", context do
    eef_advisory = %{
      id: "EEF-CVE-2026-32689",
      summary: "EEF source",
      html_url: "https://osv.dev/vulnerability/EEF-CVE-2026-32689",
      severity: :SEVERITY_HIGH,
      api_url: "https://api.osv.dev/v1/vulns/EEF-CVE-2026-32689",
      aliases: ["CVE-2026-32689", "GHSA-628h-q48j-jr6q"],
      references: []
    }

    ghsa_advisory = %{
      id: "GHSA-628h-q48j-jr6q",
      summary: "GHSA source",
      html_url: "https://osv.dev/vulnerability/GHSA-628h-q48j-jr6q",
      severity: :SEVERITY_HIGH,
      api_url: "https://api.osv.dev/v1/vulns/GHSA-628h-q48j-jr6q",
      aliases: ["CVE-2026-32689"],
      references: []
    }

    with_test_package("1.4.0", context, fn ->
      inject_advisory(@package_name, "1.4.0", [eef_advisory, ghsa_advisory])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      grouped_advisory = %{
        id: "EEF-CVE-2026-32689",
        summary: "EEF source",
        html_url: "https://osv.dev/vulnerability/EEF-CVE-2026-32689",
        severity: :SEVERITY_HIGH,
        api_url: "https://api.osv.dev/v1/vulns/EEF-CVE-2026-32689",
        aliases: [
          %{id: "CVE-2026-32689", url: :undefined},
          %{
            id: "GHSA-628h-q48j-jr6q",
            url: "https://osv.dev/vulnerability/GHSA-628h-q48j-jr6q"
          }
        ]
      }

      assert_advisory_section(@package_name, "1.4.0", grouped_advisory)

      refute_received {:mix_shell, :info,
                       ["  " <> @package_name <> " 1.4.0 - GHSA-628h-q48j-jr6q" <> _]}
    end)
  end

  test "audit (advisory ignored by primary id)", context do
    with_test_package("2.0.0", context, fn ->
      inject_advisory(@package_name, "2.0.0", [@advisory])
      Hex.State.put(:ignore_advisories, ["GHSA-test-0001"])

      Mix.Task.run("hex.audit")

      output = shell_output()
      assert output =~ "Ignored advisories:"
      assert output =~ "GHSA-test-0001"
      refute output =~ "Found packages with security advisories"
    end)
  end

  test "audit (advisory ignored by CVE alias, case-insensitively)", context do
    advisory = %{
      id: "GHSA-test-0005",
      aliases: ["CVE-2026-11111"],
      summary: "Path traversal via crafted filename",
      html_url: "https://github.com/advisories/GHSA-test-0005",
      severity: :SEVERITY_LOW,
      api_url: "https://hex.pm/api/advisories/GHSA-test-0005"
    }

    with_test_package("2.1.0", context, fn ->
      inject_advisory(@package_name, "2.1.0", [advisory])
      Hex.State.put(:ignore_advisories, ["cve-2026-11111"])

      Mix.Task.run("hex.audit")

      output = shell_output()
      assert output =~ "Ignored advisories:"
      assert output =~ "GHSA-test-0005"
      refute output =~ "Found packages with security advisories"
    end)
  end

  test "audit (retirement ignored by package name)", context do
    with_test_package("2.2.0", context, fn ->
      retire_test_package("2.2.0", "security")
      Hex.State.put(:ignore_retirements, [{@package_name, nil}])

      Mix.Task.run("hex.audit")

      output = shell_output()
      assert output =~ "Ignored retired:"
      assert output =~ "#{@package_name} 2.2.0 - (security)"
      refute output =~ "Found retired packages"
    end)
  end

  test "audit (retirement ignored by pinned version)", context do
    with_test_package("2.3.0", context, fn ->
      retire_test_package("2.3.0", "deprecated", "Use something else")
      Hex.State.put(:ignore_retirements, [{@package_name, "2.3.0"}])

      Mix.Task.run("hex.audit")

      output = shell_output()
      assert output =~ "Ignored retired:"
      refute output =~ "Found retired packages"
    end)
  end

  test "audit (pinned ignore for a different version still fails)", context do
    with_test_package("2.4.0", context, fn ->
      retire_test_package("2.4.0", "security")
      Hex.State.put(:ignore_retirements, [{@package_name, "9.9.9"}])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      output = shell_output()
      assert output =~ "Retired:"
      assert output =~ "Found retired packages"
      assert output =~ "ignore_retirements entry #{@package_name} 9.9.9 does not match"
    end)
  end

  test "audit (mixed active and ignored advisories)", context do
    other_advisory = %{
      id: "GHSA-test-0006",
      summary: "Denial of service via oversized payload",
      html_url: "https://github.com/advisories/GHSA-test-0006",
      severity: :SEVERITY_CRITICAL,
      api_url: "https://hex.pm/api/advisories/GHSA-test-0006"
    }

    with_test_package("2.5.0", context, fn ->
      inject_advisory(@package_name, "2.5.0", [@advisory, other_advisory])
      Hex.State.put(:ignore_advisories, ["GHSA-test-0001"])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      output = shell_output()
      assert output =~ "Advisories:"
      assert output =~ "GHSA-test-0006"
      assert output =~ "Ignored advisories:"
      assert output =~ "GHSA-test-0001"
      assert output =~ "Found packages with security advisories"
    end)
  end

  test "audit (stale ignore entries warn without failing)", context do
    with_test_package("2.6.0", context, fn ->
      Hex.State.put(:ignore_advisories, ["CVE-2020-00000"])
      Hex.State.put(:ignore_retirements, [{"nonexistent_package", nil}])

      Mix.Task.run("hex.audit")

      output = shell_output()
      assert output =~ "No retired or security advisory packages found"

      assert output =~
               ~s(ignore_advisories entry "CVE-2020-00000" does not match any advisory)

      assert output =~
               "ignore_retirements entry nonexistent_package does not match any retired"
    end)
  end

  test "audit (ignoring one advisory of an aliased group suppresses the group)", context do
    eef_advisory = %{
      id: "EEF-test-0001",
      aliases: ["CVE-2026-33333"],
      summary: "Remote code execution via crafted input",
      html_url: "https://cna.erlef.org/advisories/EEF-test-0001",
      severity: :SEVERITY_HIGH,
      api_url: "https://hex.pm/api/advisories/EEF-test-0001"
    }

    ghsa_advisory = %{
      id: "GHSA-test-0008",
      aliases: ["CVE-2026-33333"],
      summary: "Remote code execution via crafted input",
      html_url: "https://github.com/advisories/GHSA-test-0008",
      severity: :SEVERITY_HIGH,
      api_url: "https://hex.pm/api/advisories/GHSA-test-0008"
    }

    with_test_package("2.7.0", context, fn ->
      inject_advisory(@package_name, "2.7.0", [eef_advisory, ghsa_advisory])
      Hex.State.put(:ignore_advisories, ["EEF-test-0001"])

      Mix.Task.run("hex.audit")

      output = shell_output()
      assert output =~ "Ignored advisories:"
      refute output =~ "Found packages with security advisories"
    end)
  end

  test "policy audit flags are mutually exclusive", context do
    with_test_package("2.8.0", context, fn ->
      assert_raise Mix.Error, "--policy-overrides and --policy are mutually exclusive", fn ->
        Mix.Task.run("hex.audit", ["--policy-overrides", "--policy"])
      end
    end)
  end

  test "policy audit flags require an active policy", context do
    with_test_package("2.9.0", context, fn ->
      for flag <- ["--policy-overrides", "--policy"] do
        Mix.Task.reenable("hex.audit")

        assert_raise Mix.Error, "#{flag} requires an active dependency policy", fn ->
          Mix.Task.run("hex.audit", [flag])
        end
      end
    end)
  end

  test "policy audit fails when the configured policy cannot be loaded", context do
    with_test_package("2.10.0", context, fn ->
      Hex.State.put(:active_policy, nil)
      Hex.State.put(:policy, "hexpm:myorg/missing-policy")
      Hex.State.put(:offline, true)

      assert_raise Mix.Error, ~r/policy hexpm:myorg\/missing-policy is not cached locally/, fn ->
        Mix.Task.run("hex.audit", ["--policy"])
      end
    end)
  end

  test "--policy-overrides applies scoped advisory overrides and reports comments", context do
    accepted =
      Map.merge(@advisory, %{
        aliases: ["CVE-2026-12345"],
        id: "GHSA-test-policy-accepted"
      })

    active = %{
      @advisory
      | id: "GHSA-test-policy-active",
        summary: "A separate vulnerability remains active"
    }

    with_test_package("2.11.0", context, fn ->
      inject_advisory(@package_name, "2.11.0", [accepted, active])

      put_policy(
        restriction: %{advisory_min_severity: :SEVERITY_CRITICAL},
        overrides: [
          %{
            action: :OVERRIDE_ACTION_ADVISORY,
            ref: %{package: @package_name, requirement: "~> 2.11"},
            advisory_id: "cve-2026-12345",
            comment: "The affected parser is disabled"
          }
        ]
      )

      assert catch_throw(Mix.Task.run("hex.audit", ["--policy-overrides"])) == {:exit_code, 1}

      output = shell_output()
      assert output =~ "Advisories:"
      assert output =~ "GHSA-test-policy-active"
      assert output =~ "Policy-accepted advisories:"
      assert output =~ "GHSA-test-policy-accepted"
      assert output =~ "Policy: The affected parser is disabled"
    end)
  end

  test "--policy-overrides reports comments from ALLOW overrides", context do
    with_test_package("2.11.1", context, fn ->
      inject_advisory(@package_name, "2.11.1", [@advisory])

      put_policy(
        overrides: [
          %{
            action: :OVERRIDE_ACTION_ALLOW,
            ref: %{package: @package_name, requirement: "== 2.11.1"},
            comment: "This release was reviewed by the security team"
          }
        ]
      )

      Mix.Task.run("hex.audit", ["--policy-overrides"])

      output = shell_output()
      assert output =~ "Policy-accepted advisories:"
      assert output =~ "Policy: This release was reviewed by the security team"
      refute output =~ "Found packages with security advisories"
    end)
  end

  test "--policy applies thresholds and project ignores after policy evaluation", context do
    low = %{@advisory | id: "GHSA-test-low", severity: :SEVERITY_LOW}
    high = %{@advisory | id: "GHSA-test-high", severity: :SEVERITY_HIGH}

    with_test_package("2.12.0", context, fn ->
      inject_advisory(@package_name, "2.12.0", [low, high])
      put_policy(restriction: %{advisory_min_severity: :SEVERITY_HIGH})
      Hex.State.put(:ignore_advisories, ["GHSA-test-high"])

      Mix.Task.run("hex.audit", ["--policy"])

      output = shell_output()
      assert output =~ "Policy-accepted advisories:"
      assert output =~ "GHSA-test-low"
      assert output =~ "Accepted by the active dependency policy."
      assert output =~ "Ignored advisories:"
      assert output =~ "GHSA-test-high"
      refute output =~ "Found packages with security advisories"
    end)
  end

  test "--policy uses the highest severity in a grouped advisory", context do
    cve = "CVE-2026-23456"

    low =
      Map.merge(@advisory, %{
        id: "EEF-test-group-low",
        aliases: [cve],
        severity: :SEVERITY_LOW
      })

    critical =
      Map.merge(@advisory, %{
        id: "GHSA-test-group-critical",
        aliases: [cve],
        severity: :SEVERITY_CRITICAL
      })

    with_test_package("2.12.1", context, fn ->
      inject_advisory(@package_name, "2.12.1", [low, critical])
      put_policy(restriction: %{advisory_min_severity: :SEVERITY_HIGH})

      assert catch_throw(Mix.Task.run("hex.audit", ["--policy"])) == {:exit_code, 1}

      output = shell_output()
      assert output =~ "Advisories:"
      assert output =~ "EEF-test-group-low"
      refute output =~ "Policy-accepted advisories:"
      assert output =~ "Found packages with security advisories"
    end)
  end

  test "--policy keeps no-CVE cross-alias advisories in separate display groups", context do
    low =
      Map.merge(@advisory, %{
        id: "EEF-test-cross-alias",
        aliases: ["GHSA-test-cross-alias"],
        severity: :SEVERITY_LOW
      })

    critical =
      Map.merge(@advisory, %{
        id: "GHSA-test-cross-alias",
        aliases: [],
        severity: :SEVERITY_CRITICAL
      })

    with_test_package("2.12.2", context, fn ->
      inject_advisory(@package_name, "2.12.2", [low, critical])
      put_policy(restriction: %{advisory_min_severity: :SEVERITY_HIGH})
      Hex.State.put(:ignore_advisories, ["GHSA-test-cross-alias"])

      Mix.Task.run("hex.audit", ["--policy"])

      output = shell_output()
      assert output =~ "Policy-accepted advisories:"
      assert output =~ "EEF-test-cross-alias"
      assert output =~ "Ignored advisories:"
      assert output =~ "GHSA-test-cross-alias"
      refute output =~ "Found packages with security advisories"
    end)
  end

  test "--policy applies retirement reasons and scoped retirement overrides", context do
    with_test_package("2.13.0", context, fn ->
      retire_test_package("2.13.0", "security")

      put_policy(
        restriction: %{retirement_reasons: [:RETIRED_SECURITY]},
        overrides: [
          %{
            action: :OVERRIDE_ACTION_RETIREMENT,
            ref: %{package: @package_name},
            retirement_reason: :RETIRED_SECURITY
          }
        ]
      )

      Mix.Task.run("hex.audit", ["--policy"])

      output = shell_output()
      assert output =~ "Policy-accepted retired:"
      assert output =~ "#{@package_name} 2.13.0"
      refute output =~ "Found retired packages"
    end)
  end

  @tag :requires_json
  test "policy-accepted SARIF findings identify their source and include comments", context do
    with_test_package("2.14.0", context, fn ->
      inject_advisory(@package_name, "2.14.0", [@advisory])

      put_policy(
        restriction: %{advisory_min_severity: :SEVERITY_LOW},
        overrides: [
          %{
            action: :OVERRIDE_ACTION_ADVISORY,
            ref: %{package: @package_name},
            advisory_id: @advisory.id,
            comment: "Compensating control is active"
          }
        ]
      )

      Mix.Task.run("hex.audit", ["--policy", "--format", "sarif"])

      [result] = decode_sarif()["runs"] |> hd() |> Map.fetch!("results")
      [suppression] = result["suppressions"]
      assert suppression["justification"] == "Compensating control is active"
      assert suppression["properties"]["source"] == "policy"
    end)
  end

  @tag :requires_json
  test "audit --format sarif (retirement and advisory)", context do
    with_test_package("3.0.0", context, fn ->
      :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
        :ets.insert(
          tid,
          {{:retired, "hexpm", @package_name, "3.0.0"}, %{reason: :RETIRED_SECURITY}}
        )

        :ets.insert(tid, {{:advisories, "hexpm", @package_name, "3.0.0"}, [@advisory]})
        state
      end)

      assert catch_throw(Mix.Task.run("hex.audit", ["--format", "sarif"])) == {:exit_code, 1}

      assert_received {:mix_shell, :error, ["Found retired packages"]}
      assert_received {:mix_shell, :error, ["Found packages with security advisories"]}

      hex_version = Hex.version()
      full_name = "mix hex.audit (Hex #{hex_version})"
      summary = @advisory.summary
      advisory_url = @advisory.html_url
      full_description = "#{summary}. See #{advisory_url} for more information."

      advisory_help_text =
        "Update the affected dependency to a version that is not affected by " <>
          "GHSA-test-0001. An advisory that does not affect the project can be " <>
          "acknowledged by adding its id to the ignore_advisories list in the " <>
          ":hex section of mix.exs."

      advisory_help_markdown =
        "See [GHSA-test-0001](#{advisory_url}) for details. " <>
          String.replace(advisory_help_text, "ignore_advisories", "`ignore_advisories`")

      retired_message = "Dependency '#{@package_name}' '3.0.0' is retired: '(security)'."

      advisory_message =
        "Dependency '#{@package_name}' '3.0.0' is affected by security advisory " <>
          "'GHSA-test-0001': '#{summary}'. See '#{advisory_url}'."

      assert %{
               "$schema" => "https://json.schemastore.org/sarif-2.1.0.json",
               "version" => "2.1.0",
               "runs" => [
                 %{
                   "automationDetails" => %{
                     "id" => "hex-audit/",
                     "description" => %{"text" => "Hex dependency audit (mix hex.audit)"}
                   },
                   "originalUriBaseIds" => %{"SRCROOT" => %{"uri" => src_root}},
                   "versionControlProvenance" => [provenance],
                   "tool" => %{
                     "driver" => %{
                       "name" => "mix hex.audit",
                       "fullName" => ^full_name,
                       "informationUri" => "https://hex.pm",
                       "version" => ^hex_version,
                       "rules" => [
                         %{
                           "id" => "HEX0003",
                           "name" => "RetiredPackageSecurity",
                           "shortDescription" => %{"text" => "Hex package retired: security"},
                           "fullDescription" => %{
                             "text" =>
                               "The locked version of this dependency has been retired by " <>
                                 "its maintainers and is no longer recommended for use."
                           },
                           "helpUri" => "https://hexdocs.pm/hex/Mix.Tasks.Hex.Audit.html",
                           "help" => %{
                             "text" =>
                               "Update the dependency to a version that is not retired or " <>
                                 "switch to an alternative package. A retirement you cannot " <>
                                 "act on yet can be acknowledged by adding the package to " <>
                                 "the ignore_retirements list in the :hex section of mix.exs.",
                             "markdown" =>
                               "Update the dependency to a version that is not retired or " <>
                                 "switch to an alternative package. A retirement you cannot " <>
                                 "act on yet can be acknowledged by adding the package to " <>
                                 "the `ignore_retirements` list in the `:hex` section of " <>
                                 "`mix.exs`."
                           },
                           "defaultConfiguration" => %{"level" => "error"},
                           "messageStrings" => %{
                             "default" => %{
                               "text" => "Dependency '{0}' '{1}' is retired: '{2}'."
                             }
                           }
                         },
                         %{
                           "id" => "GHSA-test-0001",
                           "name" => "GhsaTest0001",
                           "shortDescription" => %{"text" => ^summary},
                           "fullDescription" => %{"text" => ^full_description},
                           "helpUri" => ^advisory_url,
                           "help" => %{
                             "text" => ^advisory_help_text,
                             "markdown" => ^advisory_help_markdown
                           },
                           "defaultConfiguration" => %{"level" => "error"},
                           "messageStrings" => %{
                             "default" => %{
                               "text" =>
                                 "Dependency '{0}' '{1}' is affected by security advisory " <>
                                   "'{2}': '{3}'. See '{4}'."
                             }
                           },
                           "properties" => %{
                             "tags" => ["security"],
                             "security-severity" => "8.0"
                           }
                         }
                       ]
                     }
                   },
                   "results" => [
                     %{
                       "ruleId" => "HEX0003",
                       "ruleIndex" => 0,
                       "level" => "error",
                       "message" => %{
                         "text" => ^retired_message,
                         "id" => "default",
                         "arguments" => [@package_name, "3.0.0", "(security)"]
                       },
                       "locations" => [retired_location],
                       "partialFingerprints" => %{"hexAudit/v1" => "retired:test_package"}
                     } = retired_result,
                     %{
                       "ruleId" => "GHSA-test-0001",
                       "ruleIndex" => 1,
                       "level" => "error",
                       "message" => %{
                         "text" => ^advisory_message,
                         "id" => "default",
                         "arguments" => [
                           @package_name,
                           "3.0.0",
                           "GHSA-test-0001",
                           ^summary,
                           ^advisory_url
                         ]
                       },
                       "locations" => [advisory_location],
                       "partialFingerprints" => %{
                         "hexAudit/v1" => "advisory:test_package:GHSA-test-0001"
                       }
                     } = advisory_result
                   ]
                 }
               ]
             } = decode_sarif()

      refute Map.has_key?(retired_result, "suppressions")
      refute Map.has_key?(advisory_result, "suppressions")

      # The test runs inside the Hex repository checkout, so git-derived
      # version control provenance is available.
      assert String.starts_with?(src_root, "file:///")
      assert String.ends_with?(src_root, "/")

      assert %{
               "repositoryUri" => repository_uri,
               "revisionId" => revision_id,
               "mappedTo" => %{"uriBaseId" => "SRCROOT"}
             } = provenance

      assert repository_uri =~ "://"
      assert is_binary(revision_id)

      # The lock file lives in a tmp directory inside the Hex repository
      # checkout, so its uri is relative to the repository root.
      assert %{
               "physicalLocation" => %{
                 "artifactLocation" => %{"uri" => lock_uri, "uriBaseId" => "SRCROOT"},
                 "region" => %{
                   "startLine" => 2,
                   "endLine" => 2,
                   "snippet" => %{"text" => region_snippet}
                 },
                 "contextRegion" => %{
                   "startLine" => 1,
                   "endLine" => 3,
                   "snippet" => %{"text" => context_snippet}
                 }
               }
             } = advisory_location

      assert retired_location == advisory_location
      assert String.ends_with?(lock_uri, "/mix.lock")
      assert region_snippet =~ ~s("#{@package_name}": {:hex, :#{@package_name}, "3.0.0")
      assert context_snippet =~ "%{\n"
      assert context_snippet =~ region_snippet
    end)
  end

  @tag :requires_json
  test "audit --format sarif (ignored findings are suppressed)", context do
    with_test_package("3.1.0", context, fn ->
      inject_advisory(@package_name, "3.1.0", [@advisory])
      Hex.State.put(:ignore_advisories, ["GHSA-test-0001"])

      Mix.Task.run("hex.audit", ["--format", "sarif"])

      sarif = decode_sarif()
      [run] = sarif["runs"]
      [result] = run["results"]

      assert result["ruleId"] == "GHSA-test-0001"
      assert [%{"kind" => "external"}] = result["suppressions"]
    end)
  end

  @tag :requires_json
  test "audit --format sarif --output writes a file", context do
    with_test_package("3.2.0", context, fn ->
      Mix.Task.run("hex.audit", ["--format", "sarif", "--output", "hex-audit.sarif"])

      sarif = "hex-audit.sarif" |> File.read!() |> :json.decode()
      [run] = sarif["runs"]
      assert run["results"] == []
      assert run["tool"]["driver"]["rules"] == []

      refute_received {:mix_shell, :info, ["No retired or security advisory packages found"]}
    end)
  end

  test "audit --format with an invalid format", context do
    with_test_package("3.3.0", context, fn ->
      assert_raise Mix.Error, "Invalid --format value, expected one of: human, sarif", fn ->
        Mix.Task.run("hex.audit", ["--format", "yaml"])
      end
    end)
  end

  test "audit --output without sarif format", context do
    with_test_package("3.4.0", context, fn ->
      assert_raise Mix.Error, "--output can only be used with --format sarif", fn ->
        Mix.Task.run("hex.audit", ["--output", "audit.json"])
      end
    end)
  end

  def with_test_package(version, %{auth: auth}, fun) do
    Mix.Project.push(RetiredDeps.MixProject)

    Hexpm.new_package("hexpm", @package_name, version, [], %{}, auth)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      Hex.State.put(:api_key, auth[:key])
      Mix.Dep.Lock.write(%{@package => {:hex, @package, version}})

      Mix.Task.run("deps.get")
      flush()
      fun.()
    end)
  end

  defp retire_test_package(version, reason, message \\ "") do
    Mix.Tasks.Hex.Retire.run([@package_name, version, reason, "--message", message])

    # Mix does not support the RemoteConverger.post_converge/0 callback on Elixir < 1.4,
    # so we need to explicitly reset the registry.
    Hex.Registry.Server.close()
  end

  defp inject_advisory(package, version, advisories) do
    :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
      :ets.insert(tid, {{:advisories, "hexpm", package, version}, advisories})
      state
    end)
  end

  defp put_policy(fields) do
    Hex.State.put(:active_policy, %{
      repository: "myorg",
      name: "strict-prod",
      visibility: :VISIBILITY_PUBLIC,
      repositories: [
        Map.merge(
          %{repository: "hexpm", restriction: %{}, overrides: []},
          Map.new(fields)
        )
      ]
    })
  end

  defp assert_retired_section(package, version, message) do
    header = render([:bright, "Retired:", :reset])
    row = render(["  #{package} #{version} - ", :yellow, message, :reset])

    assert_received {:mix_shell, :info, [^header]}
    assert_received {:mix_shell, :info, [^row]}
  end

  defp assert_advisory_section(package, version, advisory) do
    block =
      render([
        "  #{package} #{version} - " | Hex.Utils.format_advisory_ansi(advisory, "    ")
      ])

    assert_received {:mix_shell, :info, [^block]}
  end

  defp render(message) do
    message |> Hex.Shell.format() |> IO.iodata_to_binary()
  end

  defp decode_sarif do
    infos =
      for {:mix_shell, :info, [message]} <- flush() do
        IO.chardata_to_string(message)
      end

    assert [json] = infos
    :json.decode(json)
  end
end
