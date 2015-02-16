defmodule Mix.Tasks.Hex.Info do
  use Mix.Task

  @shortdoc "Print hex package information"

  @moduledoc """
  Prints hex package information.

  `mix hex.info PACKAGE [VERSION]`

  If `package` is given, print information about the package. This includes all
  released versions and package metadata.

  If `package` and `version` is given print release information. This includes
  remote Git URL and Git ref, and all package dependencies.
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)
    Hex.start
    Hex.Util.ensure_registry(cache: false)

    case args do
      [package] -> package(package)
      [package, version] -> release(package, version)
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.info PACKAGE [VERSION]"
    end
  end

  defp package(package) do
    case Hex.API.Package.get(package) do
      {code, body} when code in 200..299 ->
        pretty_package(body)
      {404, _} ->
        Mix.shell.error("No package with name #{package}")
      {code, body} ->
        Mix.shell.error("Failed to retrieve package information")
        Hex.Util.print_http_code(code)
        Hex.Util.print_error_result(code, body)
    end
  end

  defp release(package, version) do
    case Hex.API.Release.get(package, version) do
      {code, body} when code in 200..299 ->
        pretty_release(package, body)
      {404, _} ->
        Mix.shell.error("No release with name #{package} v#{version}")
      {code, body} ->
        Mix.shell.error("Failed to retrieve release information")
        Hex.Util.print_http_code(code)
        Hex.Util.print_error_result(code, body)
    end
  end

  defp pretty_package(package) do
    Mix.shell.info(package["name"])
    Mix.shell.info("  Releases: " <> Enum.map_join(package["releases"], ", ", &(&1["version"])))
    line_break()
    pretty_meta(package["meta"])
  end

  defp pretty_meta(meta) do
    pretty_list(meta, "contributors")
    pretty_list(meta, "licenses")
    pretty_dict(meta, "links")

    if descr = meta["description"] do
      line_break()
      Mix.shell.info(descr)
    end
  end

  defp pretty_release(package, release) do
    version = release["version"]
    Mix.shell.info(package <> " v" <> version)

    if release["has_docs"] do
      Mix.shell.info("  Documentation at: #{Hex.Util.hexdocs_url(package, version)}")
    end

    if release["requirements"] do
      Mix.shell.info("  Dependencies:")
      Enum.each(release["requirements"], fn {name, req} ->
        if req["optional"] do
          optional = " (optional)"
        end
        Mix.shell.info("    #{name}: #{req["requirement"]}#{optional}")
      end)
    end
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
      Enum.each(dict, fn {name, url} ->
        Mix.shell.info("    #{name}: #{url}")
      end)
    end
  end

  defp line_break(), do: Mix.shell.info("")
end

