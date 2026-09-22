defmodule Mix.Tasks.Hex.Audit do
  use Mix.Task
  alias Hex.Registry.Server, as: Registry

  @shortdoc "Shows retired Hex deps and security advisories for the current project"

  @moduledoc """
  Shows all Hex dependencies that have been marked as retired or have
  security advisories.

  Retired packages are no longer recommended to be used by their
  maintainers. The task will display a message describing
  the reason for retirement and exit with a non-zero code
  if any retired dependencies are found.

  Security advisories indicate known vulnerabilities in a package version.
  The task will display advisory details and exit with a non-zero code
  if any packages with advisories are found.

  ## Ignoring advisories and retirements

  Advisories that do not affect your project and retirements you cannot act
  on yet can be acknowledged in the `:hex` section of your `mix.exs` project
  configuration:

      def project() do
        [
          # ...
          hex: [
            ignore_advisories: ["CVE-2026-32686"],
            ignore_retirements: [:decimal, phoenix: "1.0.0"]
          ]
        ]
      end

  `ignore_advisories` lists advisory IDs; an advisory is ignored when the
  given ID matches its primary ID or any of its aliases, so a GHSA advisory
  can also be ignored by its CVE ID. `ignore_retirements` lists packages by
  name, optionally pinned to a single version.

  Ignored findings are listed in separate "Ignored" sections and do not cause
  a non-zero exit code. Entries that do not match anything in the lock file
  produce a warning so they can be cleaned up. The same configuration
  silences the advisory and retirement warnings printed by `mix deps.get`
  and `mix deps.update`.

  Both values can also be set with the `HEX_IGNORE_ADVISORIES` and
  `HEX_IGNORE_RETIREMENTS` environment variables as comma-separated lists,
  where retirement entries are `NAME` or `NAME@VERSION`.

  ## Command line options

    * `--policy-overrides` - reports all advisory and retirement findings except
      those accepted by a matching policy ALLOW, ADVISORY, or RETIREMENT override,
      and locked packages matched by a DENY override
    * `--policy` - reports advisory and retirement findings rejected by the
      active policy's restrictions and overrides, and locked packages matched
      by a DENY override
    * `--format FORMAT` - selects the output format. Supported formats are
      `human` (default) and `sarif`. SARIF output requires OTP 27 or later
    * `--output PATH` - writes the report to the given file instead of
      printing it. Can only be used with `--format sarif`

  Both policy modes list locked packages matched by a DENY override in a
  "Denied:" section and exit with a non-zero code when any are found.
  `ignore_advisories` and `ignore_retirements` do not apply to denied
  packages.

  With the `policy_enforce_lock` config enabled, `mix deps.get` and
  `mix deps.update` run the `--policy` check on `mix.lock` after resolving
  dependencies and fail when it reports anything. See `mix help hex.policy`.

  ## SARIF output

  With `--format sarif` the audit result is rendered as a
  [SARIF](https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html)
  v2.1.0 document that code scanning services can ingest. Each finding is
  anchored to the dependency's entry in `mix.lock`, advisory severities are
  mapped to SARIF levels and GitHub `security-severity` scores, and ignored
  findings are included with a suppression so they show up as closed
  alerts. The exit code behaves the same as with the human format, so CI
  jobs still fail while findings are open.

  ### Usage with GitHub Actions

  The report can be uploaded to [GitHub code
  scanning](https://docs.github.com/en/code-security/code-scanning), which
  lists the findings in the repository's Security tab and annotates
  `mix.lock` in pull requests:

  ```yaml
  name: Audit dependencies

  on:
    push:
      branches: [main]
    pull_request:
    schedule:
      - cron: "0 6 * * *"

  permissions:
    contents: read
    security-events: write

  jobs:
    audit:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v5
        - uses: erlef/setup-beam@v1
          with:
            otp-version: "28"
            elixir-version: "1.18"
        - run: mix deps.get
        - run: mix hex.audit --format sarif --output hex-audit.sarif
        - uses: github/codeql-action/upload-sarif@v4
          if: always()
          with:
            sarif_file: hex-audit.sarif
            category: hex-audit
  ```

  `mix hex.audit` exits with a non-zero code when it finds retired packages
  or advisories, failing the job. `if: always()` makes sure the report is
  still uploaded in that case. Add `continue-on-error: true` to the audit
  step instead if the alerts should only appear in the Security tab without
  failing the build.

  > In a project, this task must be invoked before any other tasks
  > that may load or start your application. Otherwise, you must
  > explicitly list `:hex` as part of your `:extra_applications`.
  """

  @behaviour Hex.Mix.TaskDescription

  @switches [format: :string, output: :string, policy_overrides: :boolean, policy: :boolean]

  @impl true
  def run(args) do
    Mix.Tasks.Deps.Loadpaths.run(["--no-compile", "--no-listeners"])
    Hex.start()

    {opts, _args} = OptionParser.parse!(args, strict: @switches)
    format = opts[:format] || "human"

    unless format in ["human", "sarif"] do
      Mix.raise("Invalid --format value, expected one of: human, sarif")
    end

    if opts[:output] && format != "sarif" do
      Mix.raise("--output can only be used with --format sarif")
    end

    mode = audit_mode(opts)

    Registry.open()
    policy = load_policy(mode)

    lock = Mix.Dep.Lock.read()

    lock
    |> Hex.Mix.packages_from_lock()
    |> Registry.prefetch()

    result = Hex.Audit.run(lock, policy, mode)

    case format do
      "human" -> print_human(result)
      "sarif" -> output_sarif(result, opts[:output])
    end

    warn_unused_ignores(
      result.all_retired,
      result.raw_advisories,
      Hex.State.fetch!(:ignore_advisories),
      Hex.State.fetch!(:ignore_retirements)
    )

    if Hex.Audit.active_findings?(result) do
      if format == "human", do: Hex.Shell.info("")
      if result.denied != [], do: Hex.Shell.error("Found packages denied by the active policy")
      if result.retired != [], do: Hex.Shell.error("Found retired packages")

      if result.advisories != [],
        do: Hex.Shell.error("Found packages with security advisories")

      Mix.Tasks.Hex.set_exit_code(1)
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Shows retired Hex deps and security advisories for the current project"},
      {"--policy-overrides", "Applies the active policy's overrides"},
      {"--policy", "Shows findings rejected by the active policy"},
      {"--format sarif", "Outputs the audit result as a SARIF document"}
    ]
  end

  defp audit_mode(opts) do
    case {opts[:policy_overrides], opts[:policy]} do
      {true, true} -> Mix.raise("--policy-overrides and --policy are mutually exclusive")
      {true, _policy} -> :overrides
      {_overrides, true} -> :policy
      _flags -> :default
    end
  end

  defp load_policy(:default), do: nil

  defp load_policy(mode) when mode in [:overrides, :policy] do
    case Hex.Policy.active() do
      {:ok, nil} ->
        flag = if mode == :overrides, do: "--policy-overrides", else: "--policy"
        Mix.raise("#{flag} requires an active dependency policy")

      {:ok, policy} ->
        policy
    end
  end

  defp print_human(result) do
    sections = [
      {:denied, "Denied:", result.denied, false},
      {:retired, "Retired:", result.retired, false},
      {:advisories, "Advisories:", result.advisories, false},
      {:retired, "Policy-accepted retired:", result.policy_accepted_retired, true},
      {:advisories, "Policy-accepted advisories:", result.policy_accepted_advisories, true},
      {:retired, "Ignored retired:", result.ignored_retired, false},
      {:advisories, "Ignored advisories:", result.ignored_advisories, false}
    ]

    if Enum.all?(sections, fn {_type, _header, entries, _accepted?} -> entries == [] end) do
      Hex.Shell.info("No retired or security advisory packages found")
    else
      Hex.Audit.print_sections(sections)
    end
  end

  defp output_sarif(result, output) do
    findings =
      sarif_findings(:denied, result.denied, false) ++
        sarif_findings(:retired, result.retired, false) ++
        sarif_findings(:retired, result.ignored_retired, :project) ++
        sarif_findings(:retired, result.policy_accepted_retired, :policy) ++
        sarif_findings(:advisory, result.advisories, false) ++
        sarif_findings(:advisory, result.ignored_advisories, :project) ++
        sarif_findings(:advisory, result.policy_accepted_advisories, :policy)

    lockfile = Mix.Project.config()[:lockfile] || "mix.lock"
    sarif = Hex.Sarif.encode_audit(findings, lockfile)

    case output do
      nil -> Hex.Shell.info(sarif)
      path -> File.write!(path, [sarif, ?\n])
    end
  end

  defp sarif_findings(type, entries, suppression) do
    Enum.map(entries, fn entry ->
      suppression =
        case suppression do
          :policy -> {:policy, Hex.Policy.Filter.acceptance_message(entry.acceptance)}
          other -> other
        end

      {type, entry.repo, entry.package, entry.version, entry.detail, suppression}
    end)
  end

  defp warn_unused_ignores(all_retired, raw_advisories, ignore_advisories, ignore_retirements) do
    all_advisories =
      Enum.flat_map(raw_advisories, & &1.advisories)

    advisory_warnings =
      ignore_advisories
      |> Enum.reject(fn id ->
        Enum.any?(all_advisories, &Hex.Ignores.advisory_matches?(&1, id))
      end)
      |> Enum.map(fn id ->
        "ignore_advisories entry \"#{id}\"#{ignore_source(:ignore_advisories)} does not " <>
          "match any advisory for the locked dependencies and can be removed"
      end)

    retirement_warnings =
      ignore_retirements
      |> Enum.reject(fn entry ->
        Enum.any?(all_retired, fn finding ->
          Hex.Ignores.retirement_matches?(finding.package, finding.version, entry)
        end)
      end)
      |> Enum.map(fn {name, version} ->
        "ignore_retirements entry #{format_retirement_entry(name, version)}" <>
          "#{ignore_source(:ignore_retirements)} does not match any retired locked " <>
          "dependency and can be removed"
      end)

    case advisory_warnings ++ retirement_warnings do
      [] ->
        :ok

      warnings ->
        Hex.Shell.warn("")
        Enum.each(warnings, &Hex.Shell.warn/1)
    end
  end

  defp ignore_source(key) do
    case Hex.State.fetch_source!(key) do
      {:env, var} -> " (set in environment variable #{var})"
      {:project_config, _key} -> " (set in mix.exs)"
      {:global_config, _key} -> " (set in global config)"
      _other -> ""
    end
  end

  defp format_retirement_entry(name, nil), do: name
  defp format_retirement_entry(name, version), do: "#{name} #{version}"
end
