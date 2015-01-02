defmodule Mix.Tasks.Hex.Search do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Search for package names"

  @moduledoc """
  Display packages matching the given search query.

  `mix hex.search PACKAGE`
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)
    Hex.start

    case args do
      [package] ->
        spinner = Util.start_spinner()

        Hex.Util.ensure_registry!()

        Util.stop_spinner(spinner)

        Enum.each(Hex.Registry.search(package), fn pkg ->
          Mix.shell.info(pkg)
        end)
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.search PACKAGE"
    end
  end
end
