defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Searches for package names"

  @moduledoc """
  Displays packages matching the given search query.

  `mix hex.search PACKAGE`
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)
    Hex.start

    case args do
      [package] ->
        Hex.Utils.ensure_registry!()

        packages = Hex.Registry.search(package)
        pkg_max_length = Enum.max_by(packages, &byte_size/1) |> byte_size

        Enum.each(packages, fn pkg ->
          vsn = Hex.Registry.get_versions(pkg) |> List.last
          pkg_name = String.ljust(pkg, pkg_max_length + 1)
          Hex.Shell.info "#{pkg_name}v#{vsn}"
        end)
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.search PACKAGE"
    end
  end
end
