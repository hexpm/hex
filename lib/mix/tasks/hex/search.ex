defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Searches for package names"

  @moduledoc """
  Display packages matching the given search query.

  `mix hex.search PACKAGE`
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)
    Hex.start

    case args do
      [package] ->
        Hex.Utils.ensure_registry!()

        Enum.each(Hex.Registry.search(package), fn pkg ->
          Hex.Shell.info(pkg)
        end)
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.search PACKAGE"
    end
  end
end
