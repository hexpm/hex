defmodule Mix.Tasks.Hex.Key do
  use Mix.Task
  alias Mix.Hex.Utils

  @shortdoc "Manages Hex API key"

  @moduledoc """
  Removes or lists API keys associated with your account.

  ### Remove key

  Removes given API key from account.

  The key can no longer be used to authenticate API requests.

      mix hex.key remove key_name

  To remove all API keys from your account, pass the `--all` option.

      mix hex.key remove --all

  ### List keys

  Lists all API keys associated with your account.

      mix hex.key list
  """

  @switches [all: :boolean]

  def run(args) do
    Hex.start
    {opts, args, _} = OptionParser.parse(args, switches: @switches)
    config = Hex.Config.read
    all? = Keyword.get(opts, :all, false)

    case args do
      ["remove", key] ->
        auth = Utils.auth_info(config)
        remove_key(key, auth)
      ["remove"] when all? ->
        auth = Utils.auth_info(config)
        remove_all_keys(auth)
      ["list"] ->
        auth = Utils.auth_info(config)
        list_keys(auth)
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.key remove KEY
        mix hex.key remove --all
        mix hex.key list
        """
    end
  end

  defp remove_key(key, auth) do
    Hex.Shell.info "Removing key #{key}..."
    case Hex.API.Key.delete(key, auth) do
      {200, %{"name" => ^key, "authing_key" => true}, _headers} ->
        Mix.Tasks.Hex.User.run(["deauth"])
        :ok
      {code, _body, _headers} when code in 200..299 ->
        :ok
      {code, body, _headers} ->
        Hex.Shell.error "Key removal failed"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp remove_all_keys(auth) do
    Hex.Shell.info "Removing all keys..."
    case Hex.API.Key.delete_all(auth) do
      {code, %{"name" => _, "authing_key" => true}, _headers} when code in 200..299 ->
        Mix.Tasks.Hex.User.run(["deauth"])
        :ok
      {code, body, _headers} ->
        Hex.Shell.error "Key removal failed"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp list_keys(auth) do
    case Hex.API.Key.get(auth) do
      {code, body, _headers} when code in 200..299 ->
        values = Enum.map(body, fn %{"name" => name, "inserted_at" => time} ->
          [name, time]
        end)
        Utils.print_table(["Name", "Created at"], values)
      {code, body, _headers} ->
        Hex.Shell.error "Key fetching failed"
        Hex.Utils.print_error_result(code, body)
    end
  end
end
