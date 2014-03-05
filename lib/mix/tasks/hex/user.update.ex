defmodule Mix.Tasks.Hex.User.Update do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Updates user options"

  @moduledoc """
  Updates user options.

  `mix hex.user.update -u username -p password`
  """

  @aliases [u: :user, p: :pass]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    config = Hex.Mix.read_config
    opts = Util.config_opts(opts, config)
    Util.required_opts(opts, [:user, :pass])

    Mix.shell.info("Update user options (leave blank to not change an option)")
    email    = Mix.shell.prompt("Email:")    |> String.strip |> nillify
    password = Mix.shell.prompt("Password:") |> String.strip |> nillify

    Hex.start_api
    update_user(opts[:user], email, password, opts)
  end

  defp update_user(username, email, password, auth) do
    case Hex.API.update_user(email, password, auth) do
      { 200, _ } ->
        Mix.shell.info("Updating user options for #{auth[:user]} was successful!")
        Util.update_config([username: username, password: password])
      { code, body } ->
        Mix.shell.error("Updating user options for #{auth[:user]} failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  defp nillify(""), do: nil
  defp nillify(str), do: str
end
