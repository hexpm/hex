defmodule Mix.Tasks.Hex.Policy do
  use Mix.Task

  alias Hex.Policy.{Diagnostics, Filter}

  @shortdoc "Inspects the active Hex dependency policy"

  @moduledoc """
  Shows the active Hex policy and explains why specific versions
  are blocked.

      $ mix hex.policy [show]
      $ mix hex.policy why PACKAGE

  ## Commands

    * `show` (default) — Summarize the active policy: visibility and the
      per-repository restrictions (cooldown, advisory, retirement) and
      override counts, plus the effective cooldown.
    * `why PACKAGE` — Walk every version of the named package in the
      registry, classify each against the active policy, and print a
      per-version table of status and the reasons it is blocked.

  ## Opting in

  A project opts into a single policy. It is configured like any other
  Hex setting, with the usual precedence (`HEX_POLICY` env var, then
  `mix.exs`, then `mix hex.config`):

    * `mix.exs` `:hex` block:

          defp project() do
            [hex: [policy: [repo: "myorg", name: "strict-prod"]]]
          end

    * `HEX_POLICY` env var (an `org/name` pair):

          $ HEX_POLICY=myorg/strict-prod mix deps.get

    * `mix hex.config`:

          $ mix hex.config policy myorg/strict-prod

  See https://hex.pm/docs/dependency-policies for the full guide.
  """

  @behaviour Hex.Mix.TaskDescription

  @impl true
  def run(args) do
    Hex.start()

    case args do
      [] ->
        show()

      ["show"] ->
        show()

      ["why", package] ->
        why(package)

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.policy
        mix hex.policy show
        mix hex.policy why PACKAGE
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Shows the active policy"},
      {"show", "Shows the active policy"},
      {"why PACKAGE", "Shows which versions of PACKAGE are blocked by the active policy"}
    ]
  end

  defp show() do
    case Hex.Policy.active() do
      {:ok, nil} ->
        Hex.Shell.info("No active policy configured.")

      {:ok, policy} ->
        render_show(policy)
    end
  end

  defp render_show(policy) do
    Hex.Shell.info(
      "Active policy: #{policy.repository}/#{policy.name} [#{visibility_label(policy.visibility)}]\n"
    )

    render_repositories(policy)
    Hex.Shell.info("")

    local = Hex.State.fetch!(:cooldown)

    case Diagnostics.effective_cooldown(policy, local) do
      nil ->
        :ok

      {source, duration} ->
        Hex.Shell.info("Effective cooldown: #{duration} (#{cooldown_source(source)})")
    end
  end

  defp render_repositories(p) do
    case Map.get(p, :repositories, []) do
      [] ->
        Hex.Shell.info("    (no repositories configured)")

      repositories ->
        Enum.each(repositories, &render_repository/1)
    end
  end

  defp render_repository(rp) do
    restriction = Map.get(rp, :restriction)
    overrides = Map.get(rp, :overrides, [])

    Hex.Shell.info("    #{rp.repository}:")
    Hex.Shell.info("      Cooldown:        #{cooldown_label(restriction)}")
    Hex.Shell.info("      Advisory rule:   #{advisory_label(restriction)}")
    Hex.Shell.info("      Retirement rule: #{retirement_label(restriction)}")
    Hex.Shell.info("      Overrides:       #{length(overrides)}")
  end

  defp why(arg) do
    {repo, package} = parse_package_arg(arg)

    case Hex.Policy.active() do
      {:ok, nil} ->
        Hex.Shell.info("No active policy configured.")

      {:ok, policy} ->
        Hex.Registry.Server.open()
        Hex.Registry.Server.prefetch([{repo, package}])

        case Hex.Registry.Server.versions(repo, package) do
          {:ok, versions} ->
            render_why(repo, package, versions, policy)

          :error ->
            Mix.raise("No package with name #{package} in registry")
        end
    end
  end

  defp parse_package_arg(arg) do
    case String.split(arg, "/", parts: 2) do
      [package] when byte_size(package) > 0 ->
        {"hexpm", package}

      [repo, package] when byte_size(repo) > 0 and byte_size(package) > 0 ->
        {repo, package}

      _ ->
        Mix.raise("Invalid package argument: #{inspect(arg)}; expected PACKAGE or REPO/PACKAGE")
    end
  end

  defp render_why(repo, package, versions, policy) do
    Hex.Shell.info("Versions of #{inspect(package)} (#{length(versions)}):")
    Hex.Shell.info("")

    rows =
      Enum.map(versions, fn v ->
        version = to_string(v)
        candidate = Filter.candidate_from_registry(repo, package, version)

        case Filter.classify(policy, candidate) do
          :allowed ->
            [version, "ALLOWED", ""]

          {:blocked, reasons} ->
            reason_text =
              reasons
              |> Enum.map(&Diagnostics.format_reason/1)
              |> Enum.uniq()
              |> Enum.join(", ")

            [version, "BLOCKED", reason_text]
        end
      end)

    Mix.Tasks.Hex.print_table(["Version", "Status", "Blocked by"], rows)
  end

  defp cooldown_source(:local), do: "local"
  defp cooldown_source({repo, name}), do: "#{repo}/#{name}"

  defp visibility_label(:VISIBILITY_PUBLIC), do: "public"
  defp visibility_label(:VISIBILITY_PRIVATE), do: "private"
  defp visibility_label(other), do: to_string(other)

  defp cooldown_label(%{cooldown: duration}) when is_binary(duration), do: duration
  defp cooldown_label(_restriction), do: "(none)"

  defp advisory_label(%{advisory_min_severity: :SEVERITY_NONE}), do: "block any advisory"

  defp advisory_label(%{advisory_min_severity: severity}) when not is_nil(severity),
    do: "block ≥ #{Hex.Utils.advisory_severity(severity)}"

  defp advisory_label(_restriction), do: "(disabled)"

  defp retirement_label(%{retirement_reasons: reasons}) when is_list(reasons) and reasons != [] do
    reasons
    |> Enum.map(fn r ->
      r |> Hex.Utils.package_retirement_reason() |> String.upcase()
    end)
    |> Enum.join(", ")
  end

  defp retirement_label(_restriction), do: "(disabled)"
end
