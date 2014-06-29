defmodule Mix.Tasks.Hex.Key do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Hex API key tasks"

  @moduledoc """
  Generate, remove or list API keys.

  ### Generate key

  Generate new API key and store it in the hex config.

  `mix hex.key new -u username -p password`

  ### Remove key

  Remove given API key from account.

  The key can no longer be used to authenticate API requests.

  `mix hex.key remove key_name -u username -p password`

  ### List keys

  List all API keys associated with your account.

  `mix hex.key list -u username -p password`

  ## Command line options

  * `--user`, `-u` - Username of user (required)

  * `--pass`, `-p` - Password of user (required)
  """

  @aliases [u: :user, p: :pass]

  def run(args) do
    Hex.Util.ensure_registry(fetch: false)
    Hex.start_api

    {opts, rest, _} = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :pass])

    case rest do
      ["remove", key] ->
        remove_key(key, opts)
      ["list"] ->
        list_keys(opts)
      ["new"] ->
        new_key(opts)
      _ ->
        Mix.raise "Invalid arguments, expected 'mix hex.key TASK ...'"
    end
  end

  defp new_key(opts) do
    Util.generate_key(opts[:user], opts[:pass])
  end

  defp remove_key(key, opts) do
    Mix.shell.info("Removing key #{key}...")
    case Hex.API.delete_key(key, opts) do
      {204, _body} ->
        :ok
      {code, body} ->
        Mix.shell.error("Key fetching failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp list_keys(opts) do
    case Hex.API.get_keys(opts) do
      {200, body} ->
        Enum.each(body, &Mix.shell.info(&1["name"]))
      {code, body} ->
        Mix.shell.error("Key fetching failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end
end
