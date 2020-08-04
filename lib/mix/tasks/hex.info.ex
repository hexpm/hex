defmodule Mix.Tasks.Hex.Info do
  use Mix.Task

  @shortdoc "Prints Hex information"

  @moduledoc """
  Prints Hex package or system information.

      mix hex.info [PACKAGE [VERSION]]

  If `package` is not given, print system information. This includes when
  registry was last updated and current system version.

  If `package` is given, print information about the package. This includes all
  released versions and package metadata.

  If `package` and `version` is given, print release information.

  ## Command line options

    * `--organization ORGANIZATION` - Set this for private packages belonging to an organization
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [organization: :string]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      [] ->
        general()

      [package] ->
        package(opts[:organization], package)

      [package, version] ->
        release(opts[:organization], package, version)

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.info [PACKAGE [VERSION]]
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Prints Hex information"},
      {"PACKAGE [VERSION]", "Prints package information"}
    ]
  end

  defp general() do
    Hex.Shell.info("Hex:    #{Hex.version()}")
    Hex.Shell.info("Elixir: #{System.version()}")
    Hex.Shell.info("OTP:    #{Hex.Utils.otp_version()}")
    Hex.Shell.info("")
    Hex.Shell.info("Built with: Elixir #{Hex.elixir_version()} and OTP #{Hex.otp_version()}")

    Hex.Registry.Server.open()
    Hex.UpdateChecker.check()
    Hex.Registry.Server.close()
  end

  defp package(organization, package) do
    auth = organization && Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.Package.get(organization, package, auth) do
      {:ok, {code, body, _}} when code in 200..299 ->
        print_package(body, locked_dep(package))

      {:ok, {404, _, _}} ->
        Hex.Shell.error("No package with name #{package}")

      other ->
        Hex.Shell.error("Failed to retrieve package information")
        Hex.Utils.print_error_result(other)
    end
  end

  defp release(organization, package, version) do
    auth = organization && Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.Release.get(organization, package, version, auth) do
      {:ok, {code, body, _}} when code in 200..299 ->
        print_release(organization, package, body)

      {:ok, {404, _, _}} ->
        Hex.Shell.error("No release with name #{package} #{version}")

      other ->
        Hex.Shell.error("Failed to retrieve release information")
        Hex.Utils.print_error_result(other)
    end
  end

  defp print_package(package, locked_package) do
    meta = package["meta"]
    desc = meta["description"] || "No description provided"
    Hex.Shell.info(desc <> "\n")
    releases = package["releases"] || []
    retirements = package["retirements"] || %{}
    print_config(package["name"], latest_release(releases, retirements), package["repository"])
    print_locked_package(locked_package)
    Hex.Shell.info(["Releases: "] ++ format_releases(releases, Map.keys(retirements)) ++ ["\n"])
    print_meta(meta)
  end

  @doc false
  def latest_release(releases, retirements) do
    stable_active_releases =
      Enum.filter(
        releases,
        &(Hex.Version.stable?(&1["version"]) and not (&1["version"] in retirements))
      )

    List.first(stable_active_releases) || List.first(releases)
  end

  defp format_releases(releases, retirements) do
    {releases, rest} = Enum.split(releases, 8)

    Enum.map(releases, &format_version(&1, retirements))
    |> Enum.intersperse([", "])
    |> add_ellipsis(rest)
  end

  defp format_version(%{"version" => version}, retirements) do
    if version in retirements do
      [:yellow, version, " (retired)", :reset]
    else
      [version]
    end
  end

  defp add_ellipsis(output, []), do: output
  defp add_ellipsis(output, _rest), do: output ++ [", ..."]

  defp print_meta(meta) do
    print_list(meta, "licenses")
    print_dict(meta, "links")
  end

  defp print_release(organization, package, release) do
    version = release["version"]

    print_retirement(release)
    print_config(package, release, organization)

    if release["has_docs"] do
      Hex.Shell.info("Documentation at: #{Hex.Utils.hexdocs_url(organization, package, version)}")
    end

    if requirements = release["requirements"] do
      Hex.Shell.info("Dependencies:")

      Enum.each(requirements, fn {name, req} ->
        app = req["app"]
        app = if app && app != name, do: " (app: #{app})"
        optional = if req["optional"], do: " (optional)"
        Hex.Shell.info("  #{name} #{req["requirement"]}#{app}#{optional}")
      end)
    end

    print_publisher(release)
  end

  defp print_config(name, nil, organization) do
    Hex.Shell.info("Config: " <> format_config_snippet(">= 0.0.0", name, name, organization))
  end

  defp print_config(name, release, organization) do
    app_name = String.to_atom(release["meta"]["app"] || name)
    name = String.to_atom(name)
    {:ok, version} = Hex.Version.parse(release["version"])

    snippet =
      version
      |> format_version()
      |> format_config_snippet(name, app_name, organization)

    Hex.Shell.info("Config: " <> snippet)
  end

  defp print_locked_package(nil), do: nil

  defp print_locked_package(locked_package) do
    Hex.Shell.info(["Locked version: #{locked_package.version}"])
  end

  defp format_config_snippet(version, name, name, organization)
       when organization in ["hexpm", "", nil] do
    "{#{inspect(name)}, #{inspect(version)}}"
  end

  defp format_config_snippet(version, name, name, organization) do
    "{#{inspect(name)}, #{inspect(version)}, organization: \"#{organization}\"}"
  end

  defp format_config_snippet(version, name, app_name, organization)
       when organization in ["hexpm", "", nil] do
    "{#{inspect(app_name)}, #{inspect(version)}, hex: #{inspect(name)}}"
  end

  defp format_config_snippet(version, name, app_name, organization) do
    "{#{inspect(app_name)}, #{inspect(version)}, hex: #{inspect(name)}, organization: \"#{
      organization
    }\"}"
  end

  defp format_version(%Version{major: 0, minor: minor, patch: patch, pre: []}) do
    "~> 0.#{minor}.#{patch}"
  end

  defp format_version(%Version{major: major, minor: minor, pre: []}) do
    "~> #{major}.#{minor}"
  end

  defp format_version(%Version{major: major, minor: minor, patch: patch, pre: pre}) do
    "~> #{major}.#{minor}.#{patch}#{format_pre(pre)}"
  end

  defp print_retirement(%{"retirement" => nil}), do: ""

  defp print_retirement(release) do
    retirement = %{
      reason: release["retirement"]["reason"],
      message: release["retirement"]["message"]
    }

    Hex.Shell.warn([
      [:bright, "This version has been retired"],
      [:normal, ": "],
      [:normal, Hex.Utils.package_retirement_message(retirement)]
    ])
  end

  defp print_publisher(release) do
    publisher_username = release["publisher"]["username"]

    publisher_email = release["publisher"]["email"]
    email_or_empty = if publisher_email, do: " (#{publisher_email})", else: ""

    Hex.Shell.info("Published by: #{publisher_username}#{email_or_empty}")
  end

  defp format_pre([]) do
    ""
  end

  defp format_pre(pre) do
    "-" <>
      Enum.map_join(pre, ".", fn
        int when is_integer(int) -> Integer.to_string(int)
        string when is_binary(string) -> string
      end)
  end

  defp print_list(meta, name) do
    list = Map.get(meta, name, [])

    if list != [] do
      Hex.Shell.info(String.capitalize(name) <> ": " <> Enum.join(list, ", "))
    end
  end

  defp print_dict(meta, name) do
    title = String.capitalize(name)
    dict = Map.get(meta, name, [])

    if dict != [] do
      Hex.Shell.info(title <> ":")

      Enum.each(dict, fn {key, val} ->
        Hex.Shell.info("  #{key}: #{val}")
      end)
    end
  end

  # Pull out the locked dependency version, if it exists
  defp locked_dep(package_name) do
    Mix.Dep.Lock.read()
    |> Enum.map(fn {_app, info} -> Hex.Utils.lock(info) end)
    |> Enum.find(fn locked -> locked.name == package_name end)
  end
end
