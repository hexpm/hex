defmodule Mix.Tasks.Hex.Test do
  use Mix.Task

  @aliases [u: :user, p: :pass]

  def run(args) do
    Hex.Util.ensure_registry(fetch: false)
    {opts, args, _} = OptionParser.parse(args, aliases: @aliases)

    case args do
      ["auth"] ->
        case Keyword.fetch(Hex.Util.read_config, :"#{key}") do
          {:ok, value} -> Mix.shell.info(inspect(value, pretty: true))
          :error       -> Mix.raise "Config does not contain a key #{key}"
        end
      _ ->
        Mix.raise "Invalid arguments, expected 'mix hex.test ARG'"
    end
  end
end
