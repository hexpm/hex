defmodule Mix.Tasks.Hex.User do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Hex user tasks"

  @moduledoc """
  Hex user tasks.

  ### Registers a new user

  `mix hex.user register`

  ### Prints the current user

  `mix hex.user whoami`

  ### Authorize a new user

  Authorizes a new user on the local machine by generating a new API key and
  storing it in the hex config.

  `mix hex.user auth`

  ### Deauthorize the user

  Deauthorizes the user from the local machine by removing the API key from the
  hex config.

  `mix hex.user deauth`

  ### Update user configuration

  `mix hex.user update`
  """

  @switches [clean_pass: :boolean]

  def run(args) do
    Hex.start
    Hex.Util.ensure_registry(fetch: false)

    {opts, rest, _} = OptionParser.parse(args, switches: @switches)

    case rest do
      ["register"] ->
        register(opts)
      ["whoami"] ->
        whoami()
      ["auth"] ->
        create_key(opts)
      ["deauth"] ->
        deauth()
      ["update"] ->
        update(opts)
      _ ->
        Mix.raise "Invalid arguments, expected one of:\nmix hex.user register\n" <>
                  "mix hex.user auth\nmix hex.user update\nmix hex.user whoami\nmix hex.user deauth"
    end
  end

  defp whoami() do
    config = Hex.Config.read
    case Keyword.fetch(config, :username) do
       {:ok, username} ->
        Mix.shell.info(username)
       :error ->
        Mix.raise "No user authorised on the local machine. Run `mix hex.user auth` " <>
                  "or create a new user with `mix hex.user register`"
    end
  end

  defp deauth() do
    config = Hex.Config.read
    case Keyword.fetch(config, :username) do
      {:ok, username} ->
        config
        |> Keyword.drop([:username, :key])
        |> Hex.Config.write

        Mix.shell.info "User `" <> username <> "` removed from the local machine. " <>
                       "To authenticate again, run `mix hex.user auth` " <>
                       "or create a new user with `mix hex.user register`"
      :error ->
        Mix.raise "No user authorised on the local machine. Run `mix hex.user auth` " <>
                  "or create a new user with `mix hex.user register`"
    end
  end

  defp update(opts) do
    clean? = Keyword.get(opts, :clean_pass, true)

    username = Mix.shell.prompt("Username:")          |> String.strip
    password = Util.password_get("Password:", clean?) |> String.strip

    Mix.shell.info("Update user options (leave blank to not change an option)")
    new_email    = Mix.shell.prompt("New email:")             |> String.strip |> nillify
    new_password = Util.password_get("New password:", clean?) |> String.strip |> nillify

    unless is_nil(new_password) do
      confirm = Util.password_get("New password (confirm):", clean?) |> String.strip |> nillify
      if new_password != confirm do
        Mix.raise "Entered passwords do not match"
      end
    end

    update_user(username, password, new_email, new_password)
  end

  defp update_user(username, password, new_email, new_password) do
    auth = [user: username, pass: password]

    case Hex.API.User.update(new_email, new_password, auth) do
      {code, _} when code in 200..299 ->
        Hex.Config.update([username: username])
      {code, body} ->
        Mix.shell.error("Updating user options for #{auth[:user]} failed")
        Hex.Util.print_http_code(code)
        Hex.Util.print_error_result(code, body)
    end
  end

  defp register(opts) do
    clean? = Keyword.get(opts, :clean_pass, true)

    username = Mix.shell.prompt("Username:")  |> String.strip
    email    = Mix.shell.prompt("Email:")     |> String.strip
    password = Util.password_get("Password:", clean?) |> String.strip

    unless is_nil(password) do
      confirm = Util.password_get("Password (confirm):", clean?) |> String.strip
      if password != confirm do
        Mix.raise "Entered passwords do not match"
      end
    end

    Mix.shell.info("Registering...")
    create_user(username, email, password)
  end

  defp create_user(username, email, password) do
    case Hex.API.User.new(username, email, password) do
      {code, _} when code in 200..299 ->
        Util.generate_key(username, password)
        Mix.shell.info("You are required to confirm your email to access your account, " <>
                       "a confirmation email has been sent to #{email}")
      {code, body} ->
        Mix.shell.error("Registration of user #{username} failed")
        Hex.Util.print_http_code(code)
        Hex.Util.print_error_result(code, body)
    end
  end

  defp create_key(opts) do
    clean? = Keyword.get(opts, :clean_pass, true)

    username = Mix.shell.prompt("Username:")          |> String.strip
    password = Util.password_get("Password:", clean?) |> String.strip

    Util.generate_key(username, password)
  end

  defp nillify(""), do: nil
  defp nillify(str), do: str
end
