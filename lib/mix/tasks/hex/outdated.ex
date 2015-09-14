defmodule Mix.Tasks.Hex.Outdated do
  use Mix.Task

  @shortdoc "Shows outdated Hex deps for the current project"

  @moduledoc """
  Shows all packages that have a version mismatch between the registry and
  your mix.lock file.

  By default, it only shows top-level packages explicitly listed in the
  `mix.exs` file. All outdated packages can be displayed by using the `--all`
  command line option.

  ## Command line options

    * `--all` - shows all outdated packages, including packages in `mix.lock` not specified in `mix.exs`.

  `mix hex.outdated`
  `mix hex.outdated --all`
  """

  @recursive true

  def run(args) do
    {opts, _args, _} = OptionParser.parse(args, switches: [all: :boolean])
    Hex.start
    Hex.Utils.ensure_registry!()

    lock = Mix.Dep.Lock.read

    if opts[:all] do
      lock
    else
      deps = Mix.Dep.loaded([])
      top_level = Hex.Mix.top_level(deps)
      Enum.filter(lock, fn {app, _} -> app in top_level end)
    end
    |> Enum.into(%{})
    |> Hex.Mix.from_lock
    |> Enum.map(&get_versions/1)
    |> Enum.filter(&outdated?/1)
    |> print_results
  end

  defp get_versions({package, _name, lock_version}) do
    latest_version =
      package
      |> Hex.Registry.get_versions
      |> List.last

    {package, lock_version, latest_version}
  end

  defp outdated?({_package, lock_version, latest_version}) do
    Version.compare(latest_version, lock_version) == :gt
  end

  defp print_results([]) do
    Hex.Shell.info "All packages are up-to-date."
  end
  defp print_results(packages) when is_list(packages) do
    Hex.Shell.info "Outdated packages:"

    Enum.each(packages, &print_package/1)
  end

  defp print_package({package, lock_version, latest_version}) do
    Hex.Shell.info " * #{package} (#{latest_version} > #{lock_version})"
  end
end
