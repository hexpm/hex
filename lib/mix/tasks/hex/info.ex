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
    {_opts, args, _} = OptionParser.parse(args)
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
    Hex.Shell.info "Hex:    v#{Hex.version}"
    Hex.Shell.info "Elixir: v#{System.version}"
    Hex.Shell.info "ERTS:   v#{:erlang.system_info(:version)}"
    Hex.Shell.info ""
    Hex.Shell.info "Built with: Elixir v#{Hex.elixir_version} and ERTS v#{Hex.erts_version}"
    Hex.Shell.info ""

    # Make sure to fetch registry after showing Hex version. Issues with the
    # registry should not prevent printing the version.
    Hex.Utils.ensure_registry(cache: false)
    path            = Hex.Registry.path()
    stat            = File.stat!(path)
    compressed_stat = File.stat!(path <> ".gz")
    size            = stat.size |> div(1024)
    compressed_size = compressed_stat.size |> div(1024)
    {packages, releases} = Hex.Registry.stat()

    Hex.Shell.info "Registry file available (last updated: #{format_date(stat.mtime)})"
    Hex.Shell.info "Size: #{size}kB (compressed #{compressed_size}kb)"
    Hex.Shell.info "Packages #: #{packages}"
    Hex.Shell.info "Versions #: #{releases}"
  end

  defp package(package) do
    Hex.Utils.ensure_registry(cache: false)

    case Hex.API.Package.get(package) do
      {code, body} when code in 200..299 ->
        print_package(body)
      {404, _} ->
        Hex.Shell.error "No package with name #{package}"
      {code, body} ->
        Hex.Shell.error "Failed to retrieve package information"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp release(package, version) do
    Hex.Utils.ensure_registry(cache: false)

    case Hex.API.Release.get(package, version) do
      {code, body} when code in 200..299 ->
        print_release(package, body)
      {404, _} ->
        Hex.Shell.error "No release with name #{package} v#{version}"
      {code, body} ->
        Hex.Shell.error "Failed to retrieve release information"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp print_package(package) do
    Hex.Shell.info package["name"]
    print_config(package["name"], hd(package["releases"]))
    Hex.Shell.info "  Releases: " <> Enum.map_join(package["releases"], ", ", &(&1["version"]))
    Hex.Shell.info ""
    print_meta(package["meta"])
  end

  defp print_meta(meta) do
    print_list(meta, "contributors")
    print_list(meta, "maintainers")
    print_list(meta, "licenses")
    print_dict(meta, "links")

    if descr = meta["description"] do
      Hex.Shell.info ""
      Hex.Shell.info descr
    end
  end

  defp print_release(package, release) do
    version = release["version"]
    Hex.Shell.info package <> " v" <> version
    print_config(package, release)

    if release["has_docs"] do
      # TODO: Only print this URL if we use the default API URL
      Hex.Shell.info "  Documentation at: #{Hex.Utils.hexdocs_url(package, version)}"
    end

    if requirements = release["requirements"] do
      Hex.Shell.info "  Dependencies:"
      Enum.each(requirements, fn {name, req} ->
        optional = if req["optional"], do: " (optional)"
        Hex.Shell.info "    #{name}: #{req["requirement"]}#{optional}"
      end)
    end
  end

  defp print_config(name, release) do
    app_name = release["meta"]["app"] || name
    {:ok, version} = Version.parse(release["version"])
    snippet =
      format_version(version)
      |> format_config_snippet(name, app_name)
    Hex.Shell.info "  Config: #{snippet}"
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
      Hex.Shell.info("  #{String.capitalize(name)}: " <> Enum.join(list, ", "))
    end
  end

  defp print_dict(meta, name, title \\ nil) do
    title = title || String.capitalize(name)

    if (dict = meta[name]) && dict != [] do
      Hex.Shell.info "  #{title}:"
      Enum.each(dict, fn {name, url} ->
        Hex.Shell.info "    #{name}: #{url}"
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
