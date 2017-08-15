defmodule Mix.Tasks.Hex.User do
  use Mix.Task

  @shortdoc "Manages your Hex user account"

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
    Hex.start()
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    case args do
      ["register"] ->
        register()
      ["whoami"] ->
        whoami()
      ["auth"] ->
        create_key()
      ["deauth"] ->
        deauth()
      ["passphrase"] ->
        passphrase()
      ["key"] ->
        process_key_task(opts)
      ["reset", "password"] ->
        reset_password()
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.user register
        mix hex.user whoami
        mix hex.user auth
        mix hex.user deauth
        mix hex.user passphrase
        mix hex.user key --remove-all
        mix hex.user key --remove KEY_NAME
        mix hex.user key --list
        mix hex.user reset password
        """
    end
  end

  defp process_key_task([remove_all: true]) do
    remove_all_keys()
  end
  defp process_key_task([remove: key]) do
    remove_key(key)
  end
  defp process_key_task([list: true]) do
    list_keys()
  end
  defp process_key_task(_opts) do
    Mix.raise "Invalid arguments. Run 'mix help hex.user'"
  end

  defp whoami() do
    # TODO: Support custom repo
    auth = Mix.Tasks.Hex.auth_info("hexpm")

    case Hex.API.User.me(auth) do
      {:ok, {code, body, _}} when code in 200..299 ->
        Hex.Shell.info(body["username"])
      other ->
        Hex.Shell.error("Failed to auth")
        Hex.Utils.print_error_result(other)
    end
  end

  defp reset_password() do
    name = Hex.Shell.prompt("Username or Email:") |> Hex.string_trim()

    case Hex.API.User.password_reset(name) do
      {:ok, {code, _, _}} when code in 200..299 ->
        Hex.Shell.info "We’ve sent you an email containing a link that will allow you to reset your password for the next 24 hours. " <>
                       "Please check your spam folder if the email doesn’t appear within a few minutes."
      other ->
        Hex.Shell.error("Initiating password reset for #{name} failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp deauth() do
    # TODO: Support custom repo
    Mix.Tasks.Hex.update_key("hexpm", nil)

    Hex.Shell.info "Authentication credentials removed from the local machine. " <>
                   "To authenticate again, run `mix hex.user auth` " <>
                   "or create a new user with `mix hex.user register`"
  end

  defp passphrase() do
    # TODO: Support custom repo
    encrypted_key = Hex.State.fetch!(:repos)["hexpm"].api_key

    unless encrypted_key do
      Mix.raise "No authorized user found. Run 'mix hex.user auth'"
    end

    decrypted_key = Mix.Tasks.Hex.prompt_decrypt_key(encrypted_key, "Current passphrase")
    Mix.Tasks.Hex.prompt_encrypt_key("hexpm", decrypted_key, "New passphrase")
  end

  defp register() do
    Hex.Shell.info("By registering an account on Hex.pm you accept all our " <>
                   "policies and terms of service found at https://hex.pm/policies\n")

    username = Hex.Shell.prompt("Username:") |> Hex.string_trim()
    email = Hex.Shell.prompt("Email:") |> Hex.string_trim()
    password = Mix.Tasks.Hex.password_get("Password:") |> Hex.string_trim()
    confirm = Mix.Tasks.Hex.password_get("Password (confirm):") |> Hex.string_trim()

    if password != confirm do
      Mix.raise "Entered passwords do not match"
    end

    Hex.Shell.info("Registering...")
    create_user(username, email, password)
  end

  defp create_user(username, email, password) do
    case Hex.API.User.new(username, email, password) do
      {:ok, {code, _, _}} when code in 200..299 ->
        # TODO: Support custom repo
        Mix.Tasks.Hex.generate_key("hexpm", username, password)
        Hex.Shell.info("You are required to confirm your email to access your account, " <>
                       "a confirmation email has been sent to #{email}")
      other ->
        Hex.Shell.error("Registration of user #{username} failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp create_key() do
    username = Hex.Shell.prompt("Username:") |> Hex.string_trim()
    password = Mix.Tasks.Hex.password_get("Password:") |> Hex.string_trim()

    # TODO: Support custom repo
    Mix.Tasks.Hex.generate_key("hexpm", username, password)
  end

  defp remove_all_keys() do
    auth = Mix.Tasks.Hex.auth_info("hexpm")

    Hex.Shell.info "Removing all keys..."
    case Hex.API.Key.delete_all(auth) do
      {:ok, {code, %{"name" => _, "authing_key" => true}, _headers}} when code in 200..299 ->
        Mix.Tasks.Hex.User.run(["deauth"])
      other ->
        Hex.Shell.error "Key removal failed"
        Hex.Utils.print_error_result(other)
    end
  end

  defp remove_key(key) do
    auth = Mix.Tasks.Hex.auth_info("hexpm")

    Hex.Shell.info "Removing key #{key}..."
    case Hex.API.Key.delete(key, auth) do
      {:ok, {200, %{"name" => ^key, "authing_key" => true}, _headers}} ->
        Mix.Tasks.Hex.User.run(["deauth"])
        :ok
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok
      other ->
        Hex.Shell.error "Key removal failed"
        Hex.Utils.print_error_result(other)
    end
  end

  defp list_keys() do
    auth = Mix.Tasks.Hex.auth_info("hexpm")

    case Hex.API.Key.get(auth) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        values = Enum.map(body, fn %{"name" => name, "inserted_at" => time} ->
          [name, time]
        end)
        Mix.Tasks.Hex.print_table(["Name", "Created at"], values)
      other ->
        Hex.Shell.error "Key fetching failed"
        Hex.Utils.print_error_result(other)
    end
  end
end
