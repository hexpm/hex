defmodule Mix.Tasks.Hex.Policy do
  use Mix.Task

  alias Hex.Policy.{Diagnostics, Filter}

  @shortdoc "Inspects the active Hex dependency policy"

  @moduledoc """
  Shows the active Hex policy and explains why specific versions
  are blocked.

      $ mix hex.policy show
      $ mix hex.policy why PACKAGE

  ## Commands

    * `show` — Summarize the active policy: visibility and the
      per-repository restrictions (cooldown, advisory, retirement) and
      the package overrides, plus the effective cooldown.
    * `why PACKAGE` — Walk every version of the named package in the
      registry, classify each against the active policy, and print a
      per-version table of status and the reasons it is blocked.

  ## Opting in

  A project opts into a single policy. It is configured like any other
  Hex setting, with the usual precedence (`HEX_POLICY` env var, then
  `mix.exs`, then `mix hex.config`):

    * `mix.exs` `:hex` block, with `org:` for a hexpm organization or
      `repo:` for any configured repo:

          defp project() do
            [hex: [policy: [org: "myorg", name: "strict-prod"]]]
          end

    * `HEX_POLICY` env var (a `REPO/NAME` pair). An empty value disables
      the configured policy for the invocation:

          $ HEX_POLICY=hexpm:myorg/strict-prod mix deps.get

    * `mix hex.config`:

          $ mix hex.config policy hexpm:myorg/strict-prod

  See https://hex.pm/docs/dependency-policies for the full guide.
  """

  @behaviour Hex.Mix.TaskDescription

  @impl true
  def run(args) do
    Hex.start()

    case args do
      ["show"] ->
        show()

      ["why", package] ->
        why(package)

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.policy show
        mix hex.policy why PACKAGE
        """)
    end
  end

  @impl true
  def tasks() do
    [
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
    Hex.Shell.info([
      "Active policy: ",
      [:cyan, "#{policy.repository}/#{policy.name}"],
      " ",
      visibility_tag(policy.visibility),
      "\n"
    ])

    render_repositories(policy)
    Hex.Shell.info("")

    local = Hex.State.fetch!(:cooldown)

    case Diagnostics.effective_cooldown(policy, local) do
      nil ->
        :ok

      {source, duration} ->
        Hex.Shell.info([
          "Effective cooldown: ",
          [:cyan, duration],
          " (#{cooldown_source(source)})"
        ])
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

    Hex.Shell.info(["    ", [:bright, "#{rp.repository}:"]])
    Hex.Shell.info(["      Cooldown:        ", cooldown_label(restriction)])
    Hex.Shell.info(["      Advisory rule:   ", advisory_label(restriction)])
    Hex.Shell.info(["      Retirement rule: ", retirement_label(restriction)])
    render_overrides(overrides)
  end

  defp render_overrides([]) do
    Hex.Shell.info(["      Overrides:       ", [:faint, "(none)"]])
  end

  defp render_overrides(overrides) do
    Hex.Shell.info(["      Overrides:"])

    Enum.each(overrides, fn override ->
      label =
        cond do
          Filter.valid_override?(override) ->
            override_label(override)

          Filter.unknown_override_action?(override) ->
            unknown_override_label(override)

          true ->
            [:faint, "(invalid override ignored)"]
        end

      Hex.Shell.info(["        ", label])
    end)
  end

  defp unknown_override_label(%{action: action} = override) do
    ref =
      case Map.get(override, :ref) do
        %{} = ref -> ref
        _ref -> %{}
      end

    [
      "package ",
      inspect(Map.get(ref, :package)),
      unknown_requirement_label(ref),
      "  ",
      [:yellow, "UNKNOWN ACTION #{action}"],
      " ",
      [:faint, "(ignored)"]
    ]
  end

  defp unknown_requirement_label(%{requirement: requirement}),
    do: " requirement #{inspect(requirement)}"

  defp unknown_requirement_label(_ref), do: ""

  defp override_label(%{action: action, ref: ref} = override) do
    label = override_action_label(action, override)

    case Map.get(override, :comment) do
      comment when is_binary(comment) and comment != "" ->
        [ref_label(ref), "  ", label, "  ", comment]

      _comment ->
        [ref_label(ref), "  ", label]
    end
  end

  defp override_action_label(:OVERRIDE_ACTION_ALLOW, _override), do: [:green, "ALLOW"]
  defp override_action_label(:OVERRIDE_ACTION_DENY, _override), do: [:red, "DENY"]

  defp override_action_label(:OVERRIDE_ACTION_ADVISORY, override),
    do: [:green, "ADVISORY ", override.advisory_id]

  defp override_action_label(:OVERRIDE_ACTION_RETIREMENT, override) do
    [:green, "RETIREMENT ", Hex.Utils.package_retirement_reason(override.retirement_reason)]
  end

  defp override_action_label(:OVERRIDE_ACTION_COOLDOWN, _override),
    do: [:green, "COOLDOWN"]

  defp ref_label(ref) do
    case Map.get(ref, :requirement) do
      requirement when is_binary(requirement) and requirement != "" ->
        "#{Map.get(ref, :package)} #{requirement}"

      _requirement ->
        to_string(Map.get(ref, :package))
    end
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

        case Filter.explain(policy, candidate) do
          {:allowed, acceptances} ->
            [version, [:green, "ALLOWED"], acceptance_cell(acceptances)]

          {:blocked, reasons, acceptances} ->
            reason_cell =
              reasons
              |> Enum.uniq()
              |> Enum.map(&[Diagnostics.reason_color(&1), Diagnostics.format_reason(&1)])
              |> Enum.intersperse(", ")

            details =
              case acceptance_cell(acceptances) do
                "" -> reason_cell
                accepted -> [reason_cell, "; ", accepted]
              end

            [version, [:red, "BLOCKED"], details]
        end
      end)

    Mix.Tasks.Hex.print_table(["Version", "Status", "Policy result"], rows)
  end

  defp acceptance_cell([]), do: ""

  defp acceptance_cell(acceptances) do
    acceptances
    |> Enum.map(&acceptance_label/1)
    |> Enum.intersperse(", ")
  end

  defp acceptance_label(%{kind: kind, identifier: identifier} = acceptance)
       when kind in [:advisory, :retirement] do
    label = if kind == :advisory, do: "advisory", else: "retirement"

    [
      :green,
      "#{label} #{identifier} accepted: #{Filter.acceptance_message(acceptance)}"
    ]
  end

  defp acceptance_label(%{kind: :cooldown} = acceptance),
    do: [:green, "policy cooldown bypassed: #{Filter.acceptance_message(acceptance)}"]

  defp acceptance_label(%{kind: :allow} = acceptance),
    do: [:green, "accepted by ALLOW override: #{Filter.acceptance_message(acceptance)}"]

  defp acceptance_label(%{kind: :deny} = acceptance),
    do: [:red, "denied by override: #{Filter.acceptance_message(acceptance)}"]

  defp cooldown_source(:local), do: "local"
  defp cooldown_source({repo, name}), do: "#{repo}/#{name}"

  defp visibility_tag(:VISIBILITY_PUBLIC), do: [:green, "[public]"]
  defp visibility_tag(:VISIBILITY_PRIVATE), do: [:yellow, "[private]"]
  defp visibility_tag(other), do: ["[", to_string(other), "]"]

  defp cooldown_label(%{cooldown: duration}) when is_binary(duration), do: [:cyan, duration]
  defp cooldown_label(_restriction), do: [:faint, "(none)"]

  defp advisory_label(%{advisory_min_severity: :SEVERITY_NONE}), do: [:red, "block any advisory"]

  defp advisory_label(%{advisory_min_severity: severity}) when not is_nil(severity),
    do: [:red, "block ≥ #{Hex.Utils.advisory_severity(severity)}"]

  defp advisory_label(_restriction), do: [:faint, "(disabled)"]

  defp retirement_label(%{retirement_reasons: reasons}) when is_list(reasons) and reasons != [] do
    text =
      reasons
      |> Enum.map(fn r ->
        r |> Hex.Utils.package_retirement_reason() |> String.upcase()
      end)
      |> Enum.join(", ")

    [:yellow, text]
  end

  defp retirement_label(_restriction), do: [:faint, "(disabled)"]
end
