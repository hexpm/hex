defmodule Mix.Tasks.Hex.Sponsor do
  use Mix.Task

  @shortdoc "Show Hex packages accepting sponsorships"

  @moduledoc """
  Show Hex packages in your dependencies that accept sponsorships.

  Sponsorship plays an important role to maintain some open
  source projects. This task will display all packages that
  are accepting sponsorship from the current project.

  You can add sponsorship links to your projects by adding the
  following to your mix.exs:

      links: %{
        "GitHub" => "[your-repo-link]",
        "Sponsor" => "[your-sponsorship-link]"
      }
  """

  @behaviour Hex.Mix.TaskDescription

  @impl true
  def run(_) do
    unless Mix.Project.get() do
      raise Mix.raise(
              "The sponsor task only works inside a Mix project. " <>
                "Please ensure you are in a directory with a mix.exs file."
            )
    end

    Hex.Mix.check_deps()

    sponsor_links =
      Mix.Dep.Lock.read()
      |> Hex.Mix.packages_from_lock()
      |> sponsor_links(Mix.Project.deps_path())

    case sponsor_links do
      [] ->
        Hex.Shell.info("No dependencies with sponsorship link found.")

      deps_links ->
        header = ["Dependency", "Sponsorship"]
        Mix.Tasks.Hex.print_table(header, deps_links)
    end
  end

  defp sponsor_links(packages, deps_path) do
    Enum.flat_map(packages, fn {_repo, package_name} ->
      case Hex.Sponsor.get_link(package_name, deps_path) do
        nil -> []
        value -> [[package_name, value]]
      end
    end)
  end

  @impl true
  def tasks() do
    [
      {"", @shortdoc}
    ]
  end
end
