defmodule Mix.Tasks.Hex do
  use Mix.Task

  @shortdoc "Print hex system information"

  @moduledoc """
  Prints hex system information. This includes when
  registry was last updated and current system version.

  `mix hex`
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)
    Hex.start
    Hex.Util.ensure_registry(cache: false)

    case args do
      [] -> general()
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex"
    end
  end

  defp general() do
    Mix.shell.info("Hex v" <> Hex.version)
    line_break()

    path = Hex.Registry.path()
    stat = File.stat!(path)
    {packages, releases} = Hex.Registry.stat()

    Mix.shell.info("Registry file available (last updated: #{pretty_date(stat.mtime)})")
    Mix.shell.info("Size: #{div stat.size, 1024}kB")
    Mix.shell.info("Packages #: #{packages}")
    Mix.shell.info("Versions #: #{releases}")

    if Version.match?(System.version, ">= 1.1.0-dev") do
      line_break()
      Mix.shell.info("Available tasks:")
      line_break()
      Mix.Task.run("help", ["--search", "hex."])
    end

    line_break()
    Mix.shell.info("Further information can be found here: https://hex.pm/docs/tasks")
  end

  defp pretty_date({{year, month, day}, {hour, min, sec}}) do
    "#{pad(year, 4)}-#{pad(month, 2)}-#{pad(day, 2)} " <>
    "#{pad(hour, 2)}:#{pad(min, 2)}:#{pad(sec, 2)}"
  end

  defp pad(int, padding) do
    str = to_string(int)
    padding = max(padding-byte_size(str), 0)
    do_pad(str, padding)
  end

  defp do_pad(str, 0), do: str
  defp do_pad(str, n), do: do_pad("0" <> str, n-1)

  defp line_break(), do: Mix.shell.info("")
end

