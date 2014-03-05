defmodule Mix.Tasks.Hex.User.Update do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Updates user options"

  @moduledoc """
  Updates user options.

  `mix hex.user.update -u username -p password`
  """

  @aliases [u: :user, p: :password]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :password])

    Mix.shell.info("Update user options (leave blank to not change an option)")
    email    = Mix.shell.prompt("Email:")    |> String.strip |> nillify
    password = Mix.shell.prompt("Password:") |> String.strip |> nillify

    Hex.start_api
    update_user(email, password, opts)
  end

  defp update_user(email, password, auth) do
    case Hex.API.update_user(email, password, auth) do
      { 200, _ } ->
        Mix.shell.info("Updating user options for #{auth[:user]} was successful!")
      { code, body } ->
        Mix.shell.error("Updating user options for #{auth[:user]} failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  defp nillify(""), do: nil
  defp nillify(str), do: str
end
