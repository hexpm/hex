defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Open and perform searches"

  @moduledoc """
  Open and perform searches.

  When invoked without arguments, it opens up a search page on
  https://hexdocs.pm with all of your dependencies selected:

      $ mix hex.search

  You may also pass command line flags, to execute searches
  via the command line, according to the modes below.

  ## Package search

  Specify `--package PACKAGE` to search for a given package.

      $ mix hex.search --package PACKAGE

  If you are authenticated, it will additionally search all organizations
  you are member of.

  ### Options

    * `--organization ORGANIZATION` - Set this for private packages belonging to an organization

  """
  @behaviour Hex.Mix.TaskDescription

  @switches [organization: :string, package: :string]

  @impl true
  def run(args) do
    case args do
      [] ->
        hexdocs_search()

      _ ->
        {opts, args} = OptionParser.parse!(args, strict: @switches)

        case args do
          [package] ->
            Mix.shell().error(
              "mix hex.search PACKAGE is deprecated, use --package PACKAGE instead"
            )

            package_search(package, opts[:organization])

          _ ->
            package = opts[:package]

            if is_binary(package) and args == [] do
              package_search(package, opts[:organization])
            else
              Mix.raise("""
              Invalid arguments, expected:

              mix hex.search
              mix hex.search --package PACKAGE
              """)
            end
        end
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Opens up hexdocs.pm with your dependencies"},
      {"--package PACKAGE", "Searches for package names"}
    ]
  end

  defp hexdocs_search() do
    Mix.Tasks.Deps.Loadpaths.run(["--no-compile", "--no-listeners"])
    Hex.start()

    packages =
      for {_app, info} <- Mix.Dep.Lock.read(),
          %{repo: "hexpm", name: name, version: version} <- [Hex.Utils.lock(info)] do
        "#{name}:#{version}"
      end
      |> Enum.join(",")
      |> URI.encode_www_form()

    Hex.Utils.system_open("https://hexdocs.pm/?packages=#{packages}&q=")
  end

  defp package_search(package, organization) do
    Hex.start()
    auth = Mix.Tasks.Hex.auth_info(:read, auth_inline: false)

    Hex.API.Package.search(organization, package, auth)
    |> lookup_packages()
  end

  defp lookup_packages({:ok, {200, _headers, []}}) do
    Hex.Shell.info("No packages found")
  end

  defp lookup_packages({:ok, {200, _headers, packages}}) do
    include_organizations? = Enum.any?(packages, &(&1["repository"] != "hexpm"))

    if include_organizations? do
      print_with_organizations(packages)
    else
      print_without_organizations(packages)
    end
  end

  defp lookup_packages({:ok, {_status, _headers, _body}}) do
    Hex.Shell.info("No packages found")
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
        &(Version.parse!(&1["version"]).pre == [])
      )

    version
  end

  defp trim_heredoc(nil), do: ""

  defp trim_heredoc(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map_join(" ", &String.trim/1)
  end
end
