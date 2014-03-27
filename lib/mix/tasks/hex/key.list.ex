defmodule Mix.Tasks.Hex.Key.List do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "List all API keys"

  @moduledoc """
  List all API keys associated with your account.

  `mix hex.key.list -u username -p password`

  ## Command line options

  * `--user`, `-u` - Username of user (required)

  * `--pass`, `-p` - Password of user (required)
  """

  @aliases [u: :user, p: :pass]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :pass])

    Hex.start_api

    case Hex.API.get_keys(opts) do
      { 200, body } ->
        Enum.each(body, &Mix.shell.info(&1["name"]))
      { code, body } ->
        Mix.shell.error("Key fetching failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end
end
