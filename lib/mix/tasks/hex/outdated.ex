defmodule Mix.Tasks.Hex.Outdated do
  use Mix.Task

  @shortdoc "Shows outdated hex deps for the current project"

  @moduledoc """
  Shows all packages that have a version mismatch between the
  hex.pm registry and your mix.lock file.

  By default, it only shows (top-level) packages explicitly listed in the `mix.exs` file.
  All outdated packages can be displayed by using the `--all` command line option.

  ## Command line options

    * `--all` - shows all outdated packages, including packages in `mix.lock` not specified in `mix.exs`.

  `mix hex.outdated`
  `mix hex.outdated --all`
  """

  def run(args) do
    {opts, _args, _} = OptionParser.parse(args, switches: [all: :boolean])
    Hex.start

    Hex.Util.ensure_registry!()

    case opts[:all] do
      true -> Mix.Dep.Lock.read
      _ -> Mix.Dep.Lock.read |> Enum.filter(&top_level?/1)
    end
    |> Enum.into(%{})
    |> Hex.Mix.from_lock
    |> Enum.map(&get_versions/1)
    |> Enum.filter(&outdated?/1)
    |> print_results
  end

  defp top_level?({app, _details}) do
    Mix.Dep.loaded([])
    |> Hex.Mix.top_level
    |> Enum.map(fn(dep) -> dep.app end)
    |> Enum.member?(app)
  end

  defp get_versions({package, _name, lock_version}) do
    latest_version = package
    |> Hex.Registry.get_versions
    |> Enum.reverse
    |> hd

    {package, lock_version, latest_version}
  end

  defp outdated?({_package, lock_version, latest_version}) do
    latest_version != lock_version
  end

  defp print_results([]) do
    Mix.shell.info "All packages are up-to-date."
  end
  defp print_results(packages) when is_list(packages) do
    Mix.shell.info "Outdated packages:"

    packages
    |> Enum.each(&print_package/1)
  end

  defp print_package({package, lock_version, latest_version}) do
    Mix.shell.info " * #{package} (#{latest_version} > #{lock_version})"
  end
end
