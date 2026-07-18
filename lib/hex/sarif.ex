defmodule Hex.Sarif do
  @moduledoc false

  # Renders audit findings as a SARIF v2.1.0 document
  # (https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html) so
  # they can be uploaded to services such as GitHub code scanning.
  #
  # Two note-level SARIF validator recommendations are deliberately not
  # followed:
  #
  #   * SARIF2009 (conventional rule ids): advisory rules use the advisory
  #     identifier as the rule id so each alert carries its own severity
  #     score, title and help link, matching other dependency scanners.
  #   * SARIF2002 (message.id instead of message.text): GitHub code
  #     scanning and Azure DevOps require results[].message.text and do
  #     not resolve templated messages.

  @schema "https://json.schemastore.org/sarif-2.1.0.json"
  @src_root "SRCROOT"

  # One conventionally-named rule per retirement reason. Advisory rules
  # instead use the advisory identifier as their rule id, since the rule
  # metadata (severity score, help link, description) is per advisory.
  @retired_rules %{
    RETIRED_OTHER: {"HEX0001", "RetiredPackage", "Retired Hex package"},
    RETIRED_INVALID: {"HEX0002", "RetiredPackageInvalid", "Hex package retired: invalid"},
    RETIRED_SECURITY: {"HEX0003", "RetiredPackageSecurity", "Hex package retired: security"},
    RETIRED_DEPRECATED:
      {"HEX0004", "RetiredPackageDeprecated", "Hex package retired: deprecated"},
    RETIRED_RENAMED: {"HEX0005", "RetiredPackageRenamed", "Hex package retired: renamed"}
  }
  @retired_rule_fallback @retired_rules[:RETIRED_OTHER]

  @retired_message_template "Dependency '{0}' '{1}' is retired: '{2}'."
  @advisory_message_template "Dependency '{0}' '{1}' is affected by security advisory " <>
                               "'{2}': '{3}'. See '{4}'."

  @doc """
  Encodes audit findings as a SARIF JSON document.

  Findings are `{:retired, package, version, retired, ignored?}` or
  `{:advisory, package, version, advisory, ignored?}` tuples, where
  `retired` is the retirement status map from the registry and `advisory`
  is a display group returned by
  `:mix_hex_advisory.group_for_display/1`. Ignored findings are included
  with a SARIF suppression. `lockfile` is the path of the lock file the
  results are anchored to, relative to the working directory.

  Requires the `:json` module (OTP 27 or later).
  """
  def encode_audit(findings, lockfile) do
    rules = rules(findings)

    rule_indexes =
      rules
      |> Enum.with_index()
      |> Map.new(fn {rule, index} -> {rule["id"], index} end)

    git = git_work_tree()
    artifact = artifact_location(lockfile, git)
    lock_lines = lock_lines(lockfile)
    results = Enum.map(findings, &result(&1, rule_indexes, artifact, lock_lines))

    run = %{
      # A stable category-style id: uploads with the same id update the
      # same alert set on GitHub code scanning and Azure DevOps.
      "automationDetails" => %{
        "id" => "hex-audit/",
        "description" => %{"text" => "Hex dependency audit (mix hex.audit)"}
      },
      "tool" => %{
        "driver" => %{
          "name" => "mix hex.audit",
          "fullName" => "mix hex.audit (Hex #{Hex.version()})",
          "informationUri" => "https://hex.pm",
          "version" => Hex.version(),
          "rules" => rules
        }
      },
      "results" => results
    }

    run =
      case git do
        nil ->
          run

        %{root: root} ->
          Map.put(run, "originalUriBaseIds", %{@src_root => %{"uri" => file_uri(root)}})
      end

    run =
      case version_control_provenance(git) do
        nil -> run
        provenance -> Map.put(run, "versionControlProvenance", provenance)
      end

    encode_json!(%{
      "$schema" => @schema,
      "version" => "2.1.0",
      "runs" => [run]
    })
  end

  # The git binary and the root of the work tree containing the working
  # directory, or nil when either is unavailable.
  defp git_work_tree do
    with git when is_binary(git) <- System.find_executable("git"),
         {:ok, root} <- git_cmd(git, ["rev-parse", "--show-toplevel"]) do
      %{binary: git, root: root}
    else
      _other -> nil
    end
  end

  # The artifactLocation of the lock file all results point at. Inside a
  # git work tree the uri is relative to the repository root and anchored
  # to it with uriBaseId, so consumers can map results to repository
  # files no matter which directory the task ran in.
  defp artifact_location(lockfile, git) do
    with %{root: root} <- git,
         {:ok, relative} <- relative_to_root(Path.expand(lockfile), root) do
      %{"uri" => relative, "uriBaseId" => @src_root}
    else
      _other -> %{"uri" => String.replace(lockfile, "\\", "/")}
    end
  end

  defp relative_to_root(path, root) do
    path = String.replace(path, "\\", "/")
    prefix = String.replace(root, "\\", "/") <> "/"

    if String.starts_with?(path, prefix) do
      {:ok, String.replace_prefix(path, prefix, "")}
    else
      :error
    end
  end

  defp file_uri(path) do
    path = path |> String.replace("\\", "/") |> String.trim_leading("/")
    "file:///" <> path <> "/"
  end

  # Describes the scanned revision on a best-effort basis: only when the
  # working directory is inside a git work tree and the tracked remote
  # has a URL that can be expressed as a URI.
  defp version_control_provenance(nil), do: nil

  defp version_control_provenance(%{binary: git}) do
    with {:ok, url} <- repository_url(git),
         {:ok, uri} <- repository_uri(url) do
      details = %{
        "repositoryUri" => uri,
        "mappedTo" => %{"uriBaseId" => @src_root}
      }

      details =
        case git_cmd(git, ["rev-parse", "HEAD"]) do
          {:ok, revision} -> Map.put(details, "revisionId", revision)
          :error -> details
        end

      details =
        case git_cmd(git, ["rev-parse", "--abbrev-ref", "HEAD"]) do
          # A detached HEAD has no branch
          {:ok, "HEAD"} -> details
          {:ok, branch} -> Map.put(details, "branch", branch)
          :error -> details
        end

      [details]
    else
      _other -> nil
    end
  end

  # The URL of the current branch's tracked remote, falling back to origin
  # when there is no upstream (for example on a detached HEAD).
  defp repository_url(git) do
    remote =
      case git_cmd(git, ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"]) do
        {:ok, upstream} -> upstream |> String.split("/") |> hd()
        :error -> "origin"
      end

    git_cmd(git, ["remote", "get-url", remote])
  end

  # repositoryUri must be a URI. Remote URLs in http(s)/ssh/git/file form
  # pass through (minus embedded credentials, which CI systems commonly
  # put in the remote URL and must not leak into an uploaded report);
  # scp-like remotes (git@host:path) are rewritten to their equivalent
  # ssh:// URI. Anything else, such as local paths, is dropped.
  defp repository_uri(url) do
    cond do
      url == "" -> :error
      String.contains?(url, "://") -> {:ok, strip_credentials(url)}
      scp_like?(url) -> {:ok, "ssh://" <> String.replace(url, ":", "/", global: false)}
      true -> :error
    end
  end

  defp strip_credentials(url) do
    case URI.parse(url) do
      %URI{scheme: scheme, userinfo: userinfo} = uri
      when scheme in ["http", "https"] and is_binary(userinfo) ->
        URI.to_string(%{uri | userinfo: nil})

      _other ->
        url
    end
  end

  defp scp_like?(url) do
    case :binary.split(url, ":") do
      [host, _path] -> not String.contains?(host, "/")
      _other -> false
    end
  end

  defp git_cmd(git, args) do
    case System.cmd(git, args, stderr_to_stdout: true) do
      {output, 0} -> {:ok, String.trim(output)}
      {_output, _status} -> :error
    end
  rescue
    _error -> :error
  end

  defp rules(findings) do
    retired_rules =
      findings
      |> Enum.flat_map(fn
        {:retired, _package, _version, retired, _ignored?} -> [Map.get(retired, :reason)]
        {:advisory, _package, _version, _advisory, _ignored?} -> []
      end)
      |> Enum.uniq_by(fn reason -> elem(retired_rule_info(reason), 0) end)
      |> Enum.map(&retired_rule/1)

    advisory_rules =
      findings
      |> Enum.flat_map(fn
        {:advisory, _package, _version, advisory, _ignored?} -> [advisory]
        {:retired, _package, _version, _retired, _ignored?} -> []
      end)
      |> Enum.uniq_by(& &1.id)
      |> Enum.map(&advisory_rule/1)

    retired_rules ++ advisory_rules
  end

  defp retired_rule_info(reason), do: Map.get(@retired_rules, reason, @retired_rule_fallback)

  defp retired_rule(reason) do
    {id, name, short_description} = retired_rule_info(reason)

    %{
      "id" => id,
      "name" => name,
      "shortDescription" => %{"text" => short_description},
      "fullDescription" => %{
        "text" =>
          "The locked version of this dependency has been retired by its " <>
            "maintainers and is no longer recommended for use."
      },
      "helpUri" => "https://hexdocs.pm/hex/Mix.Tasks.Hex.Audit.html",
      "help" => %{
        "text" =>
          "Update the dependency to a version that is not retired or " <>
            "switch to an alternative package. A retirement you cannot " <>
            "act on yet can be acknowledged by adding the package to the " <>
            "ignore_retirements list in the :hex section of mix.exs.",
        "markdown" =>
          "Update the dependency to a version that is not retired or " <>
            "switch to an alternative package. A retirement you cannot " <>
            "act on yet can be acknowledged by adding the package to the " <>
            "`ignore_retirements` list in the `:hex` section of `mix.exs`."
      },
      "defaultConfiguration" => %{"level" => retired_level(reason)},
      "messageStrings" => %{
        "default" => %{"text" => @retired_message_template}
      }
    }
  end

  # A retirement for security reasons points at a real vulnerability,
  # every other reason is a maintenance warning.
  defp retired_level(:RETIRED_SECURITY), do: "error"
  defp retired_level(_reason), do: "warning"

  defp advisory_rule(advisory) do
    properties =
      case security_severity(advisory) do
        nil -> %{"tags" => ["security"]}
        score -> %{"tags" => ["security"], "security-severity" => score}
      end

    rule = %{
      "id" => advisory.id,
      "name" => rule_name(advisory.id),
      "shortDescription" => %{"text" => advisory.summary},
      "fullDescription" => %{"text" => advisory_full_description(advisory)},
      "help" => advisory_help(advisory),
      "defaultConfiguration" => %{"level" => advisory_level(advisory)},
      "messageStrings" => %{
        "default" => %{"text" => @advisory_message_template}
      },
      "properties" => properties
    }

    case advisory do
      %{html_url: url} -> Map.put(rule, "helpUri", url)
      _other -> rule
    end
  end

  defp result({:retired, package, version, retired, ignored?}, rule_indexes, artifact, lock_lines) do
    reason = Map.get(retired, :reason)
    {rule_id, _name, _short_description} = retired_rule_info(reason)
    message = Hex.Utils.package_retirement_message(retired)

    %{
      "ruleId" => rule_id,
      "ruleIndex" => Map.fetch!(rule_indexes, rule_id),
      "level" => retired_level(reason),
      "message" => result_message(@retired_message_template, [package, version, message]),
      "locations" => [location(package, artifact, lock_lines)],
      "partialFingerprints" => %{"hexAudit/v1" => "retired:#{package}"}
    }
    |> put_suppressions(ignored?)
  end

  defp result(
         {:advisory, package, version, advisory, ignored?},
         rule_indexes,
         artifact,
         lock_lines
       ) do
    %{
      "ruleId" => advisory.id,
      "ruleIndex" => Map.fetch!(rule_indexes, advisory.id),
      "level" => advisory_level(advisory),
      "message" => advisory_result_message(package, version, advisory),
      "locations" => [location(package, artifact, lock_lines)],
      "partialFingerprints" => %{"hexAudit/v1" => "advisory:#{package}:#{advisory.id}"}
    }
    |> put_suppressions(ignored?)
  end

  defp advisory_help(advisory) do
    remediation =
      "Update the affected dependency to a version that is not affected by " <>
        "#{advisory.id}. An advisory that does not affect the project can be " <>
        "acknowledged by adding its id to the ignore_advisories list in the " <>
        ":hex section of mix.exs."

    markdown =
      case advisory do
        %{html_url: url} ->
          "See [#{advisory.id}](#{url}) for details. " <>
            String.replace(remediation, "ignore_advisories", "`ignore_advisories`")

        _other ->
          remediation
      end

    %{"text" => remediation, "markdown" => markdown}
  end

  # The registry only carries an advisory summary, so the comprehensive
  # description is the summary plus a pointer to the advisory page.
  defp advisory_full_description(advisory) do
    text =
      if String.ends_with?(advisory.summary, [".", "!", "?"]) do
        advisory.summary
      else
        advisory.summary <> "."
      end

    case advisory do
      %{html_url: url} -> text <> " See " <> url <> " for more information."
      _other -> text
    end
  end

  # PascalCase name derived from the advisory id, for example
  # "GHSA-1234-abcd" becomes "Ghsa1234Abcd".
  defp rule_name(id) do
    id
    |> String.split(["-", "_", ".", ":", "/"], trim: true)
    |> Enum.map_join(&String.capitalize/1)
  end

  defp put_suppressions(result, false), do: result

  defp put_suppressions(result, true) do
    Map.put(result, "suppressions", [
      %{
        "kind" => "external",
        "justification" => "Ignored by the ignore_advisories/ignore_retirements Hex configuration"
      }
    ])
  end

  defp advisory_result_message(package, version, advisory) do
    id =
      case advisory do
        %{aliases: [_ | _] = aliases} ->
          "#{advisory.id} (aka #{Enum.map_join(aliases, ", ", &alias_id/1)})"

        _other ->
          advisory.id
      end

    result_message(@advisory_message_template, [
      package,
      version,
      id,
      advisory.summary,
      advisory_url(advisory)
    ])
  end

  defp advisory_url(%{html_url: url}), do: url
  defp advisory_url(advisory), do: "https://osv.dev/vulnerability/#{advisory.id}"

  # A result message carrying both the resolved text (required by GitHub
  # and Azure DevOps) and the template reference with its arguments (for
  # consumers that resolve messages through the rule's messageStrings).
  defp result_message(template, arguments) do
    text =
      arguments
      |> Enum.with_index()
      |> Enum.reduce(template, fn {argument, index}, text ->
        String.replace(text, "{#{index}}", argument)
      end)

    %{"text" => text, "id" => "default", "arguments" => arguments}
  end

  defp alias_id(%{id: id}), do: id
  defp alias_id(id) when is_binary(id), do: id

  defp advisory_level(%{severity: severity})
       when severity in [:SEVERITY_CRITICAL, :SEVERITY_HIGH],
       do: "error"

  defp advisory_level(%{severity: :SEVERITY_MEDIUM}), do: "warning"

  defp advisory_level(%{severity: severity}) when severity in [:SEVERITY_LOW, :SEVERITY_NONE],
    do: "note"

  defp advisory_level(_advisory), do: "warning"

  # GitHub buckets `security-severity` scores as critical >= 9.0,
  # high >= 7.0, medium >= 4.0 and low < 4.0. Advisories only carry a
  # severity level, so map each level to a representative score.
  defp security_severity(%{severity: :SEVERITY_CRITICAL}), do: "9.5"
  defp security_severity(%{severity: :SEVERITY_HIGH}), do: "8.0"
  defp security_severity(%{severity: :SEVERITY_MEDIUM}), do: "5.5"
  defp security_severity(%{severity: :SEVERITY_LOW}), do: "3.0"
  defp security_severity(%{severity: :SEVERITY_NONE}), do: "0.0"
  defp security_severity(_advisory), do: nil

  defp location(package, artifact, lock_lines) do
    physical_location =
      case Map.get(lock_lines.packages, package) do
        nil ->
          %{"artifactLocation" => artifact}

        line ->
          %{
            "artifactLocation" => artifact,
            "region" => %{
              "startLine" => line,
              "endLine" => line,
              "snippet" => %{"text" => elem(lock_lines.lines, line - 1)}
            },
            "contextRegion" => context_region(lock_lines.lines, line)
          }
      end

    %{"physicalLocation" => physical_location}
  end

  # The result line plus up to one surrounding line on each side.
  defp context_region(lines, line) do
    first = max(line - 1, 1)
    last = min(line + 1, tuple_size(lines))

    snippet = Enum.map_join(first..last, "\n", &elem(lines, &1 - 1))

    %{
      "startLine" => first,
      "endLine" => last,
      "snippet" => %{"text" => snippet}
    }
  end

  # The lock file's lines plus a map from each locked package to its line
  # number, found by looking for the `{:hex, :package_name, ...}` tuples.
  # Keyed by package name rather than the lock entry's app name since
  # findings carry package names.
  defp lock_lines(lockfile) do
    case File.read(lockfile) do
      {:ok, contents} ->
        lines =
          contents
          |> String.split("\n")
          |> Enum.map(&String.trim_trailing/1)

        lines =
          case Enum.reverse(lines) do
            ["" | rest] -> Enum.reverse(rest)
            _other -> lines
          end

        packages =
          lines
          |> Enum.with_index(1)
          |> Enum.reduce(%{}, fn {line, index}, acc ->
            case package_from_line(line) do
              nil -> acc
              package -> Map.put_new(acc, package, index)
            end
          end)

        %{lines: List.to_tuple(lines), packages: packages}

      {:error, _reason} ->
        %{lines: {}, packages: %{}}
    end
  end

  defp package_from_line(line) do
    with [_before, rest] <- :binary.split(line, "{:hex, :"),
         [package, _rest] <- :binary.split(rest, ",") do
      package
    else
      _other -> nil
    end
  end

  defp encode_json!(term) do
    if Code.ensure_loaded?(:json) do
      term |> :json.encode() |> IO.iodata_to_binary()
    else
      Mix.raise(":json module is not available, upgrade OTP to use this feature")
    end
  end
end
