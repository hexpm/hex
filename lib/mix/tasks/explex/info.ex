defmodule Mix.Tasks.Explex.Info do
  use Mix.Task
  alias Mix.Tasks.Explex.Util

  def run(args) do
    { _opts, args, _ } = OptionParser.parse(args)
    Explex.start_api

    case args do
      [] -> general()
      [package] -> package(package)
      [package, version] -> release(package, version)
      _ ->
        raise Mix.Error, message: "invalid arguments, expected 'mix explex.info [ PACKAGE [ VERSION ] ]'"
    end
  end

  defp general() do
    # TODO: version, new version avaliable?
    #       github links to both repos (for docs)

    path = Explex.Registry.path()

    if File.exists?(path) do
      Explex.start_mix
      stat = File.stat!(path)
      { packages, releases } = Explex.Registry.stat()

      Mix.shell.info("Registry file available (last updated: #{pretty_date(stat.mtime)})")
      Mix.shell.info("Size: #{div stat.size, 1024}kB")
      Mix.shell.info("Packages #: #{packages}")
      Mix.shell.info("Releases #: #{releases}")
    else
      Mix.shell.info("No registry file available, fetch it with 'mix explex.update'")
    end
  end

  defp package(package) do
    case Explex.API.get_package(package) do
      { 200, body } ->
        pretty_package(body)
      { 404, _ } ->
        Mix.shell.error("No package with name #{package}")
      { code, body } ->
        Mix.shell.error("Failed to retrieve package information! (#{code})")
        Util.print_error_result(body)
    end
  end

  defp release(package, version) do
    case Explex.API.get_release(package, version) do
      { 200, body } ->
        pretty_release(package, body)
      { 404, _ } ->
        Mix.shell.error("No release with package name #{package} and version #{version}")
      { code, body } ->
        Mix.shell.error("Failed to retrieve release information! (#{code})")
        Util.print_error_result(body)
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
    if repo = release["git_url"], do: Mix.shell.info("  Git repo: #{repo}")
    if ref = release["git_ref"], do: Mix.shell.info("  Git ref: #{ref}")
    pretty_dict(release, "requirements", "Dependencies")
  end

  defp pretty_list(meta, name) do
    if (list = meta[name]) && list != [] do
      Mix.shell.info("  #{String.capitalize(name)}: " <> Enum.join(list, ", "))
    end
  end

  # defp pretty_dict(meta, name, title \\ String.capitalize(name)) do
  defp pretty_dict(meta, name, title \\ nil) do
    title = title || name

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
