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
        search_package(package, opts[:organization])

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.search PACKAGE
        """)
    end
  end

  defp search_package(package, organization) do
    result =
      if key = Hex.State.fetch!(:api_key) do
        decrypted_key = Mix.Tasks.Hex.prompt_decrypt_key(key)
        Hex.API.Package.search(organization, package, key: decrypted_key)
      else
        Hex.API.Package.search(organization, package)
      end

    lookup_packages(result)
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
