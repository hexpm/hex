defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Searches for package names"

  @moduledoc """
  Displays packages matching the given search query.

      mix hex.search PACKAGE

  ## Command line options

    * `--organization ORGANIZATION` - The organization the package belongs to
    * `--all-organizations` - Search all organizations you have access to

  """

  @switches [organization: :string, all_organizations: :boolean]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      [package] ->
        search_package(package, opts[:organization], opts[:all_organizations] || false)

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.search PACKAGE
        """)
    end
  end

  defp search_package(package, organization, all_organizations?) do
    auth = if organization || all_organizations?, do: Mix.Tasks.Hex.auth_info()
    organization = select_organization(organization, all_organizations?)

    Hex.API.Package.search(organization, package, auth)
    |> lookup_packages()
  end

  defp select_organization(_organization, true), do: nil
  defp select_organization(nil, false), do: "hexpm"
  defp select_organization(organization, false), do: organization

  defp lookup_packages({:ok, {403, _body, _headers}}) do
    Hex.Shell.error("Organization not found or not authorized for it")
  end

  defp lookup_packages({:ok, {200, [], _headers}}) do
    Hex.Shell.info("No packages found")
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
    string |> String.split("\n", trim: true) |> Enum.map_join(" ", &(&1 |> Hex.string_trim()))
  end
end
