defmodule Mix.Tasks.Hex.Docs.Search do
  use Mix.Task

  @shortdoc "Searches hexdocs, returning JSON"

  @moduledoc """
  Searches hexdocs, returning JSON.

  If no version is specified, defaults to version used in the current mix project.
  If called outside of a mix project or the dependency is not used in the
  current mix project, defaults to the latest version.

  ## Search documentation for all dependencies in the current mix project

      $ mix hex.docs.search "search term"

  ## Search documentation for specific packages 

      $ mix hex.docs.search "search term" -p ecto -p ash

  ## Search documentation for specific versions 

      $ mix hex.docs.search "search term" -p ecto@3.13.2 -p ash@3.5.26
  """
  @behaviour Hex.Mix.TaskDescription

  @elixir_apps ~w(eex elixir ex_unit iex logger mix)
  @switches [package: :keep]
  @aliases [p: :package]

  @impl true
  def run([]) do
    Mix.raise("""
    Must provide a search term. For example:

        $ mix hex.docs.search "search term"
    """)
  end

  def run([term | args]) do
    Hex.start()
    {opts, args} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)
    opts = Keyword.put(opts, :mix_project, !!Mix.Project.get())

    filter_by =
      case opts[:packages] do
        p when p in [nil, []] ->
          filter_from_mix_lock()

        packages ->
          filter_from_packages(packages)
      end

    query_params =
      %{
        q: term,
        query_by: "doc,title",
        filter_by: filter_by
      }

    case Hex.API.Search.search(query_params) do
      {:ok, 200, _, body} ->
        Mix.shell().info(body)

      error ->
        # todo do this better 
        Mix.raise("""
        Docs search failed.

        #{inspect(error)}
        """)
    end
  end

  defp filter_from_mix_lock do
    apps =
      if apps_paths = Mix.Project.apps_paths() do
        Enum.filter(Mix.Project.deps_apps(), &is_map_key(apps_paths, &1))
      else
        [Mix.Project.config()[:app]]
      end

    filter =
      apps
      |> Enum.flat_map(fn app ->
        Application.load(app)
        Application.spec(app, :applications)
      end)
      |> Enum.uniq()
      |> Enum.map(fn app ->
        "#{app}-#{Application.spec(app, :vsn)}"
      end)
      |> Enum.join(", ")

    "package:=[#{filter}]"
  end

  defp filter_from_packages(packages) do
    filter =
      packages
      |> Enum.flat_map(fn package ->
        case Hex.API.Package.get(nil, package) do
          {:ok, %{status: 200, body: body}} ->
            ["#{package}-#{get_latest_version(body)}"]

          other ->
            Logger.warning(
              "Failed to get latest version for package #{package}: #{inspect(other)}"
            )

            []
        end
      end)
      |> Enum.join(", ")

    "package:=[#{filter}]"
  end

  defp get_latest_version(package) do
    versions =
      for release <- package["releases"],
          version = Version.parse!(release["version"]),
          # ignore pre-releases like release candidates, etc.
          version.pre == [] do
        version
      end

    Enum.max(versions, Version)
  end
end
