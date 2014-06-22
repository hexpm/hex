defmodule Mix.Tasks.Hex.User do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Hex user tasks"

  @moduledoc """
  Hex user tasks.

  ### Registers a new hex user.

  `mix hex.user.register`

  ### Update user options.

  `mix hex.user.update -u username -p password`

  ## Command line options

  * `--user`, `-u` - Username of user (required)

  * `--pass`, `-p` - Password of user (required)
  """

  @aliases [u: :user, p: :pass]
  @switches [clean_pass: :boolean]

  def run(args) do
    { opts, rest, _ } = OptionParser.parse(args, aliases: @aliases, switches: @switches)

    case rest do
      ["register"] ->
        register(opts)
      ["update"] ->
        update(opts)
      _ ->
        Mix.raise "Invalid arguments, expected 'mix hex.user task'"
    end
  end

  defp update(opts) do
    Util.required_opts(opts, [:user, :pass])
    clean? = Keyword.get(opts, :clean_pass, false)

    Mix.shell.info("Update user options (leave blank to not change an option)")
    email    = Mix.shell.prompt("Email:")             |> String.strip |> nillify
    password = Util.password_get("Password:", clean?) |> String.strip |> nillify

    unless nil?(password) do
      confirm = Util.password_get("Password (confirm):", clean?) |> String.strip |> nillify
      if password != confirm do
        Mix.raise "Entered passwords do not match"
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

  defp register(opts) do
    clean? = Keyword.get(opts, :clean_pass, false)

    username = Mix.shell.prompt("Username:")  |> String.strip
    email    = Mix.shell.prompt("Email:")     |> String.strip
    password = Util.password_get("Password:", clean?) |> String.strip

    unless nil?(password) do
      confirm = Util.password_get("Password (confirm):", clean?) |> String.strip
      if password != confirm do
        Mix.raise "Entered passwords do not match"
      end
    end

    Mix.shell.info("Registering...")
    Hex.start_api
    create_user(username, email, password)
  end

  defp create_user(username, email, password) do
    case Hex.API.new_user(username, email, password) do
      { 201, _ } ->
        Util.generate_key(username, password)
      { code, body } ->
        Mix.shell.error("Registration of user #{username} failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp nillify(""), do: nil
  defp nillify(str), do: str
end
