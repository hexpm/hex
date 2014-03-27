defmodule Mix.Tasks.Hex.Key.Drop do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Drop an API key"

  @moduledoc """
  Remove given API key from account. The key can no longer be used to
  authenticate API requests.

  `mix hex.key.drop key_name -u username -p password`

  ## Command line options

  * `--user`, `-u` - Username of user (required)

  * `--pass`, `-p` - Password of user (required)
  """

  @aliases [u: :user, p: :pass]

  def run(args) do
    { opts, rest, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :pass])

    case rest do
      [key] ->
        drop_key(key, opts)
      _ ->
        raise Mix.Error, message: "invalid arguments, expected 'mix hex.key.drop key_name'"
    end

    Hex.start_api
    Util.generate_key(opts[:user], opts[:pass])
  end

  defp drop_key(key, opts) do
    case Hex.API.delete_key(key, opts) do
      { 204, _body } ->
        Mix.shell.info("Key #{key} dropped!")
      { code, body } ->
        Mix.shell.error("Key fetching failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end
end
