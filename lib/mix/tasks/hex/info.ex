defmodule Mix.Tasks.Hex.Info do
  use Mix.Task

  @shortdoc "Prints Hex information"

  @moduledoc """
  Prints Hex package or system information.

  `mix hex.info [PACKAGE [VERSION]]`

  If `package` is not given, print system information. This includes when
  registry was last updated and current system version.

  If `package` is given, print information about the package. This includes all
  released versions and package metadata.

  If `package` and `version` is given print release information. This includes
  remote Git URL and Git ref, and all package dependencies.
  """

  def run(args) do
    Hex.start

    case args do
      [] -> general()
      [package] -> package(package)
      [package, version] -> release(package, version)
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.info [PACKAGE [VERSION]]"
    end
  end

  defp general() do
    Hex.Shell.info "Hex:    #{Hex.version}"
    Hex.Shell.info "Elixir: #{System.version}"
    Hex.Shell.info "OTP:    #{Hex.Utils.otp_version}\n"
    Hex.Shell.info "Built with: Elixir #{Hex.elixir_version} and OTP #{Hex.otp_version}\n"

    # Make sure to fetch registry after showing Hex version. Issues with the
    # registry should not prevent printing the version.
    {fetch_time, _} = :timer.tc fn -> Hex.Utils.ensure_registry!(cache: false, open: false) end
    {load_time, _}  = :timer.tc fn -> Hex.Registry.open(Hex.Registry.ETS) end
    path            = Hex.Registry.ETS.path
    stat            = File.stat!(path, time: :local)
    stat_gz         = File.stat!(path <> ".gz", time: :local)
    file_size       = stat.size |> div(1024)
    file_gz_size    = stat_gz.size |> div(1024)
    mem_size        = Hex.Registry.ETS.memory |> div(1024)
    {packages, releases} = Hex.Registry.stat()

    Hex.Shell.info "Registry file available (last updated: #{format_date(stat.mtime)})"
    Hex.Shell.info "File size:   #{file_size}kB (compressed #{file_gz_size}kb)"
    Hex.Shell.info "Memory size: #{mem_size}kB"
    Hex.Shell.info "Fetch time:  #{div fetch_time, 1000}ms"
    Hex.Shell.info "Load time:   #{div load_time, 1000}ms"
    Hex.Shell.info "Packages #:  #{packages}"
    Hex.Shell.info "Versions #:  #{releases}"
  end

  defp package(package) do
    Hex.Utils.ensure_registry(cache: false)

    case Hex.API.Package.get(package) do
      {code, body, _} when code in 200..299 ->
        print_package(body)
      {404, _, _} ->
        Hex.Shell.error "No package with name #{package}"
      {code, body, _} ->
        Hex.Shell.error "Failed to retrieve package information"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp release(package, version) do
    Hex.Utils.ensure_registry(cache: false)

    case Hex.API.Release.get(package, version) do
      {code, body, _} when code in 200..299 ->
        print_release(package, body)
      {404, _, _} ->
        Hex.Shell.error "No release with name #{package} #{version}"
      {code, body, _} ->
        Hex.Shell.error "Failed to retrieve release information"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp print_package(package) do
    meta = package["meta"]
    desc = meta["description"] || "No description provided"
    Hex.Shell.info desc <> "\n"
    rels = package["releases"]
    print_config(package["name"], hd(rels))
    Hex.Shell.info "Releases: " <> format_releases(rels) <> "\n"
    print_meta(meta)
  end

  defp format_releases(releases) do
    {releases, rest} = Enum.split(releases, 10)
    Enum.map_join(releases, ", ", &(&1["version"])) <>
    if(rest != [], do: ", ..." , else: "")
  end

  defp print_meta(meta) do
    print_list(meta, "contributors")
    print_list(meta, "maintainers")
    print_list(meta, "licenses")
    print_dict(meta, "links")
  end

  defp print_release(package, release) do
    version = release["version"]
    print_config(package, release)

    if release["has_docs"] do
      # TODO: Only print this URL if we use the default API URL
      Hex.Shell.info "Documentation at: #{Hex.Utils.hexdocs_url(package, version)}"
    end

    if requirements = release["requirements"] do
      Hex.Shell.info "Dependencies:"
      Enum.each(requirements, fn {name, req} ->
        optional = if req["optional"], do: " (optional)"
        app = if (app = req["app"]) && app != name, do: " (app: #{app})"
        Hex.Shell.info "  #{name} #{req["requirement"]}#{app}#{optional}"
      end)
    end
  end

  defp print_config(name, release) do
    app_name = release["meta"]["app"] || name
    {:ok, version} = Hex.Version.parse(release["version"])
    snippet =
      format_version(version)
      |> format_config_snippet(name, app_name)
    Hex.Shell.info "Config: " <> snippet
  end

  defp format_config_snippet(version, name, name),
    do: "{:#{name}, \"#{version}\"}"
  defp format_config_snippet(version, name, app_name),
    do: "{:#{app_name}, \"#{version}\", hex: :#{name}}"

  defp format_version(%Version{major: 0, minor: minor, patch: patch, pre: []}),
    do: "~> 0.#{minor}.#{patch}"
  defp format_version(%Version{major: major, minor: minor, pre: []}),
    do: "~> #{major}.#{minor}"
  defp format_version(%Version{major: major, minor: minor, patch: patch, pre: pre}),
    do: "~> #{major}.#{minor}.#{patch}#{format_pre(pre)}"

  defp format_pre([]), do: ""
  defp format_pre(pre) do
    "-" <>
      Enum.map_join(pre, ".", fn
        int when is_integer(int) -> Integer.to_string(int)
        string when is_binary(string) -> string
      end)
  end

  defp print_list(meta, name) do
    if (list = meta[name]) && list != [] do
      Hex.Shell.info(String.capitalize(name) <> ": " <> Enum.join(list, ", "))
    end
  end

  defp print_dict(meta, name) do
    title = String.capitalize(name)

    if (dict = meta[name]) && dict != [] do
      Hex.Shell.info title <> ":"
      Enum.each(dict, fn {key, val} ->
        Hex.Shell.info "  #{key}: #{val}"
      end)
    end
  end

  defp format_date({{year, month, day}, {hour, min, sec}}) do
    "#{pad0(year, 4)}-#{pad0(month, 2)}-#{pad0(day, 2)} " <>
    "#{pad0(hour, 2)}:#{pad0(min, 2)}:#{pad0(sec, 2)}"
  end

  defp pad0(int, padding) do
    str = to_string(int)
    padding = max(padding - byte_size(str), 0)
    String.duplicate("0", padding) <> str
  end
end
