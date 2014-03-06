defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Search for package names"

  @moduledoc """
  Display packages matching the given search query.

  `mix hex.search package`
  """

  def run(args) do
    { _opts, args, _ } = OptionParser.parse(args)

    case args do
      [package] ->
        Hex.start

        Enum.each(Hex.Registry.search(package), fn pkg ->
          Mix.shell.info(pkg)
        end)
      _ ->
        raise Mix.Error, message: "invalid arguments, expected 'mix hex.search package'"
    end
  end
end
