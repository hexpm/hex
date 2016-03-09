defmodule Mix.Tasks.Hex.Registry do
  use Mix.Task

  @shortdoc "Hex registry tasks"

  @moduledoc """
  Hex registry tasks.

  ### Fetch registry

  Updates registry file.

  `mix hex.registry fetch`

  ### Dump registry

  Copies registry file to the given path.

  `mix hex.registry dump <path>`

  ### Load registry

  Takes registry file from the given path.

  `mix hex.registry load <path>`

  The purpose of dumping and loading registry file
  is to assist with isolate dependency resolution.
  There is `HEX_OFFLINE` environment variable available
  to run Hex in offline mode.
  """

  def run(args) do
    Hex.start

    case args do
      ["fetch"] ->
        fetch()
      ["dump", path] ->
        dump(path)
      ["load", path] ->
        load(path)
      _otherwise ->
        message = """
          Invalid arguments, expected one of:
            mix hex.registry fetch
            mix hex.registry dump <path>
            mix hex.registry load <path>
          """
        Mix.raise message
    end
  end

  defp fetch() do
    Hex.Utils.ensure_registry!(update: true)
  end

  defp dump(dest) do
    path_gz = Hex.Registry.ETS.path <> ".gz"
    File.cp!(path_gz, dest)
  end

  defp load(source) do
    path = Hex.Registry.ETS.path
    path_gz = path <> ".gz"
    content = File.read!(source) |> :zlib.gunzip
    File.cp!(source, path_gz)
    File.write!(path, content)
  end
end
