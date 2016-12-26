defmodule Mix.Tasks.Hex.User do
  use Mix.Task
  alias Mix.Hex.Utils

  @shortdoc "Registers or manages Hex user"

  @moduledoc """
  Hex user tasks.

  ### Register a new user

      mix hex.user register

  ### Print the current user

      mix hex.user whoami

  ### Authorize a new user

  Authorizes a new user on the local machine by generating a new API key and
  storing it in the Hex config.

      mix hex.user auth

  ### Deauthorize the user

  Deauthorizes the user from the local machine by removing the API key from the
  Hex config.

      mix hex.user deauth

  ### Reencrypt API key

  Updates the passphrase for the locally stored API key.

      mix hex.user passphrase

  ### Remove key

  Removes given API key from account.

  The key can no longer be used to authenticate API requests.

      mix hex.user key --remove key_name

  ### Remove all keys

  Remove all API keys from your account.

      mix hex.user key --remove-all

  ### List keys

  Lists all API keys associated with your account.

      mix hex.user key --list

  ### Test authentication

  Tests if authentication works with the stored API key.

      mix hex.user test

  ### Reset user password

      mix hex.user reset password
  """

  @switches [remove_all: :boolean, remove: :string, list: :boolean]

  def run(args) do
    Hex.start
    config = Hex.Config.read()
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    case args do
      ["register"] ->
        register()
      ["whoami"] ->
        whoami(config)
      ["auth"] ->
        create_key()
      ["deauth"] ->
        deauth(config)
      ["passphrase"] ->
        passphrase(config)
      ["reset", "password"] ->
        reset_password()
      ["test"] ->
        test(config)
      ["key"] ->
        process_key_task(opts, config)
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.user register
        mix hex.user auth
        mix hex.user whoami
        mix hex.user deauth
        mix hex.user reset password
        mix hex.user key --remove-all
        mix hex.user key --remove KEY_NAME
        mix hex.user key --list
        """
    end
  end

  defp process_key_task([remove_all: true], config), do: remove_all_keys(config)
  defp process_key_task([remove: key], config), do: remove_key(key, config)
  defp process_key_task([list: true], config), do: list_keys(config)

  defp whoami(config) do
    username = local_user(config)
    Hex.Shell.info(username)
  end

  defp reset_password do
    name = Hex.Shell.prompt("Username or Email:") |> Hex.string_trim

    case Hex.API.User.password_reset(name) do
      {code, _, _} when code in 200..299 ->
        Hex.Shell.info "We’ve sent you an email containing a link that will allow you to reset your password for the next 24 hours. " <>
                       "Please check your spam folder if the email doesn’t appear within a few minutes."
      {code, body, _} ->
        Hex.Shell.error("Initiating password reset for #{name} failed")
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp deauth(config) do
    username = local_user(config)

    config
    |> Keyword.drop([:username, :key, :key_cipher, :key_salt, :encrypted_key])
    |> Hex.Config.write

    Hex.Shell.info "User `" <> username <> "` removed from the local machine. " <>
                   "To authenticate again, run `mix hex.user auth` " <>
                   "or create a new user with `mix hex.user register`"
  end

  defp passphrase(config) do
    key = cond do
      encrypted_key = config[:encrypted_key] ->
        Utils.decrypt_key(encrypted_key, "Current passphrase")
      key = config[:key] ->
        key
      true ->
        Mix.raise "No authorized user found. Run 'mix hex.user auth'"
    end

    Utils.encrypt_key(config, key, "New passphrase")
  end

  defp register do
    Hex.Shell.info("By registering an account on Hex.pm you accept all our " <>
                   "policies and terms of service found at https://hex.pm/policies\n")

    username = Hex.Shell.prompt("Username:") |> Hex.string_trim
    email    = Hex.Shell.prompt("Email:") |> Hex.string_trim
    password = Utils.password_get("Password:") |> Hex.string_trim
    confirm  = Utils.password_get("Password (confirm):") |> Hex.string_trim

    if password != confirm do
      Mix.raise "Entered passwords do not match"
    end

    Hex.Shell.info("Registering...")
    create_user(username, email, password)
  end

  defp create_user(username, email, password) do
    case Hex.API.User.new(username, email, password) do
      {code, _, _} when code in 200..299 ->
        Utils.generate_key(username, password)
        Hex.Shell.info("You are required to confirm your email to access your account, " <>
                       "a confirmation email has been sent to #{email}")
      {code, body, _} ->
        Hex.Shell.error("Registration of user #{username} failed")
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp create_key do
    username = Hex.Shell.prompt("Username:") |> Hex.string_trim
    password = Utils.password_get("Password:") |> Hex.string_trim

    Utils.generate_key(username, password)
  end

  defp remove_all_keys(config) do
    auth = Utils.auth_info(config)

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

  defp remove_key(key, config) do
    auth = Utils.auth_info(config)

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

  defp list_keys(config) do
    auth = Utils.auth_info(config)

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

  # TODO
  defp test(config) do
    username = local_user(config)
    auth = Utils.auth_info(config)

    case Hex.API.User.test(username, auth) do
      {code, _, _} when code in 200..299 ->
        Hex.Shell.info("Successfully authed. Your key works.")
      {code, body, _} ->
        Hex.Shell.error("Failed to auth")
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp local_user(config) do
    case Keyword.fetch(config, :username) do
      {:ok, username} ->
        username
      :error ->
        Mix.raise "No user authorised on the local machine. Run `mix hex.user auth` " <>
                  "or create a new user with `mix hex.user register`"
    end
  end
end
