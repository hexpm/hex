defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Searches for package names"

  @moduledoc """
  Displays packages matching the given search query.

      mix hex.search PACKAGE

  ## Command line options

    * `--organization ORGANIZATION` - The organization the package belongs to
  """

  @switches [organization: :string]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      [package] ->
        Hex.API.Package.search(opts[:organization], package)
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
        [
          package["name"],
          Hex.Utils.truncate(package["meta"]["description"] |> trim_heredoc),
          latest_stable(package["releases"]),
          package["html_url"] || package["url"]
        ]
      end)

    Mix.Tasks.Hex.print_table(
      ["Package", "Description", "Version", "URL"],
      values
    )
  end

  defp latest_stable(releases) do
    %{"version" => version} = Enum.find(
      releases,
      %{"version" => nil},
      &is_stable/1
    )

    version
  end

  defp is_stable(%{"version" => version}) do
    parsed_version = Hex.Version.parse!(version)
    parsed_version.pre == []
  end

  defp trim_heredoc(string) do
    string |> String.split("\n", trim: true) |> Enum.map_join(" ", &(&1 |> Hex.string_trim()))
  end
end
