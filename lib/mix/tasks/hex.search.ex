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
          latest(package["releases"]),
          latest_stable(package["releases"]),
          package["html_url"] || package["url"]
        ]
      end)

    Mix.Tasks.Hex.print_table(
      ["Package", "Description", "Latest", "Stable", "URL"],
      values
    )
  end

  defp latest([%{"version" => version} | _]) do
    version
  end

  defp latest_stable(releases) do
    %{"version" => version} = releases
    |> Enum.find(fn %{"version" => version} ->
      case Version.parse(version) do
        {:ok, parsed_version} -> parsed_version.pre == []
        _ -> false
      end
    end)

    version
  end

  defp trim_heredoc(string) do
    string |> String.split("\n", trim: true) |> Enum.map_join(" ", &(&1 |> Hex.string_trim()))
  end
end
