defmodule Mix.Tasks.Hex.Key do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Hex API key tasks"

  @moduledoc """
  Remove or list API keys associated with your account.

  ### Remove key

  Remove given API key from account.

  The key can no longer be used to authenticate API requests.

  `mix hex.key remove key_name`

  ### List keys

  List all API keys associated with your account.

  `mix hex.key list`
  """

  def run(args) do
    Hex.start
    Hex.Util.ensure_registry(fetch: false)

    auth = Util.auth_info()

    case args do
      ["remove", key] ->
        remove_key(key, auth)
      ["list"] ->
        list_keys(auth)
      _ ->
        Mix.raise "Invalid arguments, expected one of:\nmix hex.key remove KEY\nmix hex.key list'"
    end
  end

  defp remove_key(key, auth) do
    Mix.shell.info("Removing key #{key}...")
    case Hex.API.Key.delete(key, auth) do
      {204, _body} ->
        :ok
      {code, body} ->
        Mix.shell.error("Key fetching failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp list_keys(auth) do
    case Hex.API.Key.get(auth) do
      {200, body} ->
        Enum.each(body, &Mix.shell.info(&1["name"]))
      {code, body} ->
        Mix.shell.error("Key fetching failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end
end
