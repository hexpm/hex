defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Search for package names"

  @moduledoc """
  Display packages matching the given search query.

  `mix hex.search PACKAGE`
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)

    case args do
      [package] ->
        Hex.start
        Hex.Util.ensure_registry!()

        Enum.each(Hex.Registry.search(package), fn pkg ->
          Mix.shell.info(pkg)
        end)
      _ ->
        Mix.raise "Invalid arguments, expected 'mix hex.search PACKAGE'"
    end
  end
end
