defmodule Mix.Tasks.Hex.Search do
  use Mix.Task
  alias Mix.Hex.Utils

  @shortdoc "Searches for package names"

  @moduledoc """
  Displays packages matching the given search query.

      mix hex.search PACKAGE
  """

  def run(args) do
    Hex.start

    # TODO: Use API

    case args do
      [package] ->
        Hex.API.Package.search(package)
        |> lookup_packages

      _ ->
        Mix.raise """
        Invalid arguments, expected:
        mix hex.search PACKAGE
        """
    end
  end

  defp lookup_packages({200, [], _headers}) do
    Hex.Shell.info "No packages found"
  end
  defp lookup_packages({200, packages, _headers}) do
    values =
      Enum.map(packages, fn package ->
        [package["name"], package["url"]]
      end)

    Utils.table(["Package", "URL"], values)
  end
end
