defmodule Mix.Tasks.Hex.User.Update do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Update user options"

  @moduledoc """
  Update user options.

  `mix hex.user.update -u username -p password`

  ## Command line options

  * `--user`, `-u` - Username of user (required)

  * `--pass`, `-p` - Password of user (required)
  """

  @aliases [u: :user, p: :pass]
  @switches [clean_pass: :boolean]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases, switches: @switches)
    Util.required_opts(opts, [:user, :pass])
    clean? = opts[:clean_pass]

    Mix.shell.info("Update user options (leave blank to not change an option)")
    email    = Mix.shell.prompt("Email:")             |> String.strip |> nillify
    password = Util.password_get("Password:", clean?) |> String.strip |> nillify

    unless nil?(password) do
      confirm = Util.password_get("Password (confirm):", clean?) |> String.strip |> nillify
      if password != confirm do
        raise Mix.Error, message: "Entered passwords do not match"
      end
    end

    Hex.start_api
    update_user(opts[:user], email, password, opts)
  end

  defp update_user(username, email, password, auth) do
    case Hex.API.update_user(email, password, auth) do
      { 200, _ } ->
        Util.update_config([username: username, password: password])
      { code, body } ->
        Mix.shell.error("Updating user options for #{auth[:user]} failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp nillify(""), do: nil
  defp nillify(str), do: str
end
