defmodule Mix.Tasks.Hex.Info do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Print hex package or system information"

  @moduledoc """
  Prints hex package or system information.

  `mix hex.info [package [version]]`

  If `package` is not given, print system information. This includes when
  registry was last updated, current system version and if there is a new
  version available.

  If `package` is given, print information about the package. This includes all
  released versions and package metadata.

  If `package` and `version` is given print release information. This includes
  remote Git URL and Git ref, and all package dependencies.
  """

  def run(args) do
    { _opts, args, _ } = OptionParser.parse(args)
    Hex.start_api

    case args do
      [] -> general()
      [package] -> package(package)
      [package, version] -> release(package, version)
      _ ->
        raise Mix.Error, message: "invalid arguments, expected 'mix hex.info [package [version]]'"
    end
  end

  defp general() do
    Mix.shell.info("Hex v" <> Hex.version)

    case Hex.API.get_installs() do
      { 200, body } ->
        # Default to stable channel when we have one
        latest = body[Hex.channel] || body["dev"]

        if Version.compare(latest["version"], Hex.version) == :gt do
          Mix.shell.info("Newer version available: v" <> latest["version"] <> " update with 'mix hex.update --system'")
        else
          Mix.shell.info("Latest version installed")
        end

      { code, body } ->
        Mix.shell.error("Failed to fetch installation information (#{code})")
        Util.print_error_result(code, body)
    end

    Mix.shell.info("")
    path = Hex.Registry.path()
    if File.exists?(path) do
      Hex.start_mix
      stat = File.stat!(path)
      { packages, releases } = Hex.Registry.stat()

      Mix.shell.info("Registry file available (last updated: #{pretty_date(stat.mtime)})")
      Mix.shell.info("Size: #{div stat.size, 1024}kB")
      Mix.shell.info("Packages #: #{packages}")
      Mix.shell.info("Releases #: #{releases}")
    else
      Mix.shell.info("No registry file available, fetch it with 'mix hex.update'")
    end
  end

  defp package(package) do
    case Hex.API.get_package(package) do
      { 200, body } ->
        pretty_package(body)
      { 404, _ } ->
        Mix.shell.error("No package with name #{package}")
      { code, body } ->
        Mix.shell.error("Failed to retrieve package information! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  defp release(package, version) do
    case Hex.API.get_release(package, version) do
      { 200, body } ->
        pretty_release(package, body)
      { 404, _ } ->
        Mix.shell.error("No release with package name #{package} and version #{version}")
      { code, body } ->
        Mix.shell.error("Failed to retrieve release information! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  defp pretty_package(package) do
    Mix.shell.info(package["name"])
    Mix.shell.info("  Releases: " <> Enum.map_join(package["releases"], ", ", &(&1["version"])))
    Mix.shell.info("")
    pretty_meta(package["meta"])
  end

  defp pretty_meta(meta) do
    pretty_list(meta, "contributors")
    pretty_list(meta, "licenses")
    pretty_dict(meta, "links")

    if descr = meta["description"] do
      Mix.shell.info("")
      Mix.shell.info(descr)
    end
  end

  defp pretty_release(package, release) do
    Mix.shell.info(package <> " v" <> release["version"])
    pretty_dict(release, "requirements", "Dependencies")
  end

  defp pretty_list(meta, name) do
    if (list = meta[name]) && list != [] do
      Mix.shell.info("  #{String.capitalize(name)}: " <> Enum.join(list, ", "))
    end
  end

  defp pretty_dict(meta, name, title \\ nil) do
    title = title || String.capitalize(name)

    if (dict = meta[name]) && dict != [] do
      Mix.shell.info("  #{title}:")
      Enum.each(dict, fn { name, url } ->
        Mix.shell.info("    #{name}: #{url}")
      end)
    end
  end

  defp pretty_date({ { year, month, day }, { hour, min, sec } }) do
    "#{pad(year, 4)}-#{pad(month, 2)}-#{pad(day, 2)} " <>
    "#{pad(hour, 2)}:#{pad(min, 2)}:#{pad(sec, 2)}"
  end

  defp pad(int, padding) do
    str = to_string(int)
    padding = max(padding-byte_size(str), 0)
    do_pad(str, padding)
  end

  defp do_pad(str, 0), do: str
  defp do_pad(str, n), do: do_pad("0" <> str, n-1)
end
