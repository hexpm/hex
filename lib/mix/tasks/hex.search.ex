defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Searches for package names"

  @moduledoc """
  Displays packages matching the given search query.

      mix hex.search PACKAGE

  ## Command line options

    * `--repo REPOSITORY` - The repository to communicate with (default: hexpm)
  """

  @switches [repo: :string]

  def run(args) do
    Hex.start()
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    case args do
      [package] ->
        Hex.API.Package.search(opts[:repo], package)
        |> lookup_packages()

      _ ->
        Mix.raise """
        Invalid arguments, expected:
        mix hex.search PACKAGE
        """
    end
  end

  defp lookup_packages({:ok, {200, [], _headers}}) do
    Hex.Shell.info "No packages found"
  end
  defp lookup_packages({:ok, {200, packages, _headers}}) do
    values =
      Enum.map(packages, fn package ->
        [package["name"], latest(package["releases"]), package["html_url"] || package["url"]]
      end)

    Mix.Tasks.Hex.print_table(["Package", "Version", "URL"], values)
  end

  defp latest([%{"version" => version} | _]) do
    version
  end
end
