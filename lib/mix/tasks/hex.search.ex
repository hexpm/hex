defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Searches for package names"

  @moduledoc """
  Displays packages matching the given search query.

  If you are authenticated it will additionally search all organizations you are member of.

      mix hex.search PACKAGE

  ## Command line options

    * `--organization ORGANIZATION` - Search packages in specific organization

  """

  @switches [organization: :string, all_organizations: :boolean]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      [package] ->
        search_package(package, opts[:organization])

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.search PACKAGE
        """)
    end
  end

  defp search_package(package, organization) do
    auth = Mix.Tasks.Hex.auth_info(:read, auth_inline: false)

    Hex.API.Package.search(organization, package, auth)
    |> lookup_packages()
  end

  defp lookup_packages({:ok, {200, [], _headers}}) do
    Hex.Shell.info("No packages found")
  end

  defp lookup_packages({:ok, {200, packages, _headers}}) do
    include_organizations? = Enum.any?(packages, & &1["repository"] != "hexpm")

    if include_organizations? do
      print_with_organizations(packages)
    else
      print_without_organizations(packages)
    end
  end

  defp print_with_organizations(packages) do
    values =
      Enum.map(packages, fn package ->
        [
          if(package["repository"] != "hexpm", do: package["repository"]),
          package["name"],
          package["meta"]["description"] |> trim_heredoc() |> Hex.Utils.truncate(),
          latest_stable(package["releases"]),
          package["html_url"] || package["url"]
        ]
      end)

    Mix.Tasks.Hex.print_table(
      ["Organization", "Package", "Description", "Version", "URL"],
      values
    )
  end

  defp print_without_organizations(packages) do
    values =
      Enum.map(packages, fn package ->
        [
          package["name"],
          package["meta"]["description"] |> trim_heredoc() |> Hex.Utils.truncate(),
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
    %{"version" => version} =
      Enum.find(
        releases,
        %{"version" => nil},
        &Hex.Version.stable?(&1["version"])
      )

    version
  end

  defp trim_heredoc(nil), do: ""

  defp trim_heredoc(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map_join(" ", &Hex.string_trim/1)
  end
end
