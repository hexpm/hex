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

  ### Update user options

  `mix hex.user update`

  """

  @switches [clean_pass: :boolean]

  def run(args) do
    Hex.Util.ensure_registry(fetch: false)
    Hex.start_api

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
    case Keyword.fetch(Hex.Util.read_config, :username) do
       {:ok, username} ->
        Mix.shell.info(username)
       :error ->
        Mix.raise "No user authorised on the local machine. Run `mix hex.user auth` " <>
                  "or create a new user with `mix hex.user register`"
    end
  end

  defp deauth() do
    case Keyword.fetch(Hex.Util.read_config, :username) do
      {:ok, username} ->
        Hex.Util.read_config
        |> Keyword.drop([:username, :key])
        |> Hex.Util.write_config

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
    new_email    = Mix.shell.prompt("Email:")             |> String.strip |> nillify
    new_password = Util.password_get("Password:", clean?) |> String.strip |> nillify

    unless is_nil(new_password) do
      confirm = Util.password_get("Password (confirm):", clean?) |> String.strip |> nillify
      if password != confirm do
        Mix.raise "Entered passwords do not match"
      end
    end

    update_user(username, password, new_email, new_password)
  end

  defp update_user(username, password, new_email, new_password) do
    auth = [user: username, pass: password]

    case Hex.API.User.update(new_email, new_password, auth) do
      {200, _} ->
        Hex.Util.update_config([username: username])
      {code, body} ->
        Mix.shell.error("Updating user options for #{auth[:user]} failed (#{code})")
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
      {201, _} ->
        Util.generate_key(username, password)
      {code, body} ->
        Mix.shell.error("Registration of user #{username} failed (#{code})")
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
