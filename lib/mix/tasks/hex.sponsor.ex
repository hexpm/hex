defmodule Mix.Tasks.Hex.Sponsor do
  use Mix.Task

  @shortdoc "Show Hex packages accepting sponsorships"

  @moduledoc """
  Show Hex packages in your dependencies that accept sponsorships.

  Sponsorship plays an important role to maintain some open
  source projects. This task will display all packages that
  are accepting sponsorship from the current project.
  """

  @behaviour Hex.Mix.TaskDescription
  @metadata_file "hex_metadata.config"
  @sponsor_link_name "sponsor"

  @impl true
  def run(_) do
    Hex.Mix.check_deps()

    sponsor_links =
      Mix.Dep.Lock.read()
      |> Hex.Mix.packages_from_lock()
      |> sponsor_links(Mix.Project.deps_path())

    case sponsor_links do
      [] ->
        Hex.Shell.info("No dependencies with sponsorship link found")

      deps_links ->
        header = ["Dependency", "Sponsorship"]
        Mix.Tasks.Hex.print_table(header, deps_links)
    end
  end

  defp sponsor_links(packages, deps_path) do
    Enum.flat_map(packages, fn {_repo, package_name} ->
      case read_metadata(package_name, deps_path) do
        {:ok, metadata} ->
          case sponsorship(metadata) do
            {_, value} ->
              [[package_name, value]]

            _ ->
              []
          end

        _ ->
          []
      end
    end)
  end

  defp read_metadata(package, deps_path) do
    :file.consult(Path.join([deps_path, package, @metadata_file]))
  end

  defp sponsorship(metadata) do
    case Enum.find(metadata, fn {name, _} -> name == "links" end) do
      {_links, links} ->
        Enum.find(links, fn {link, _} -> String.downcase(link) == @sponsor_link_name end)

      _ ->
        nil
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Shows Hex deps accepting sponsorship from current project"}
    ]
  end
end
