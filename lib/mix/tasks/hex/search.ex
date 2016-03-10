defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Searches for package names"

  @moduledoc """
  Displays packages matching the given search query.

  `mix hex.search PACKAGE`
  """

  def run(args) do
    Hex.start

    case args do
      [package] ->
        Hex.Utils.ensure_registry!()

        Hex.PackageRegistry.search(package)
        |> lookup_packages

      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.search PACKAGE"
    end
  end

  defp lookup_packages([]) do
    Hex.Shell.info "No packages found"
  end
  defp lookup_packages(packages) do
    pkg_max_length = Enum.max_by(packages, &byte_size/1) |> byte_size

    Enum.each(packages, fn pkg ->
      vsn = Hex.PackageRegistry.get_versions(pkg) |> List.last
      pkg_name = String.ljust(pkg, pkg_max_length)
      Hex.Shell.info "#{pkg_name} #{vsn}"
    end)
  end
end
