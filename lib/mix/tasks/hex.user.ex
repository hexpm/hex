defmodule Mix.Tasks.Hex.User do
  use Mix.Task

  @shortdoc "Manages your Hex user account"

  @moduledoc """
  Hex user tasks.

  ## Register a new user

      mix hex.user register

  ## Print the current user

      mix hex.user whoami

  ## Authorize a new user

  Authorizes a new user on the local machine by generating a new API key and
  storing it in the Hex config.

      mix hex.user auth [--key-name KEY_NAME]

  ## Command line options

    * `--key-name KEY_NAME` - By default Hex will base the key name on your machine's
      hostname, use this option to give your own name.

  ## Deauthorize the user

  Deauthorizes the user from the local machine by removing the API key from the Hex config.

      mix hex.user deauth

  ## Generate API key

  Generates an unencrypted API key for your account

      mix hex.user key --generate

  ## Command line options

    * `--key-name KEY_NAME` - By default Hex will base the key name on your machine's
      hostname, use this option to give your own name.

  ## Revoke key

  Removes given API key from account.

  The key can no longer be used to authenticate API requests.

      mix hex.user key --revoke KEY_NAME

  ## Revoke all keys

  Revoke all API keys from your account.

      mix hex.user key --revoke-all

  ## List keys

  Lists all API keys associated with your account.

      mix hex.user key --list

  ## Reset user account password

  Starts the process for reseting account password.

      mix hex.user reset_password account

  ## Reset local password

  Updates the local password for your local authentication credentials.

      mix hex.user reset_password local
  """

  @switches [
    revoke_all: :boolean,
    revoke: :string,
    list: :boolean,
    key_name: :string,
    generate: :boolean
  ]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      ["register"] ->
        register()

      ["whoami"] ->
        whoami()

      ["auth"] ->
        auth(opts)

      ["deauth"] ->
        deauth()

      ["key"] ->
        process_key_task(opts)

      ["reset_password", "account"] ->
        reset_account_password()

      ["reset_password", "local"] ->
        reset_local_password()

      _ ->
        invalid_args()
    end
  end

  defp invalid_args() do
    Mix.raise("""
    Invalid arguments, expected one of:

    mix hex.user register
    mix hex.user whoami
    mix hex.user auth
    mix hex.user deauth
    mix hex.user key --generate
    mix hex.user key --revoke-all
    mix hex.user key --revoke KEY_NAME
    mix hex.user key --list
    mix hex.user reset_password account
    mix hex.user reset_password local
    """)
  end

  defp process_key_task(opts) do
    cond do
      opts[:revoke_all] ->
        revoke_all_keys()

      key = opts[:revoke] ->
        revoke_key(key)

      opts[:list] ->
        list_keys()

      opts[:generate] ->
        generate_unencrypted_key(opts)

      true ->
        invalid_args()
    end
  end

  defp whoami() do
    auth = Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.User.me(auth) do
      {:ok, {code, body, _}} when code in 200..299 ->
        Hex.Shell.info(body["username"])

      other ->
        Hex.Shell.error("Failed to auth")
        Hex.Utils.print_error_result(other)
    end
  end

  defp reset_account_password() do
    name = Hex.Shell.prompt("Username or Email:") |> Hex.string_trim()

    case Hex.API.User.password_reset(name) do
      {:ok, {code, _, _}} when code in 200..299 ->
        Hex.Shell.info(
          "We’ve sent you an email containing a link that will allow you to reset " <>
            "your account password for the next 24 hours. Please check your spam folder if the " <>
            "email doesn’t appear within a few minutes."
        )

      other ->
        Hex.Shell.error("Initiating password reset for #{name} failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp reset_local_password() do
    encrypted_key = Hex.State.fetch!(:api_key_write)

    unless encrypted_key do
      Mix.raise("No authorized user found. Run `mix hex.user auth`")
    end

    decrypted_key = Mix.Tasks.Hex.prompt_decrypt_key(encrypted_key, "Current local password")
    Mix.Tasks.Hex.prompt_encrypt_key(decrypted_key, "New local password")
  end

  defp deauth() do
    Mix.Tasks.Hex.update_keys(nil, nil)
    deauth_organizations()

    Hex.Shell.info(
      "Authentication credentials removed from the local machine. " <>
        "To authenticate again, run `mix hex.user auth` " <>
        "or create a new user with `mix hex.user register`"
    )
  end

  defp deauth_organizations() do
    Hex.State.fetch!(:repos)
    |> Enum.reject(fn {name, _config} -> String.starts_with?(name, "hexpm:") end)
    |> Enum.into(%{})
    |> Hex.Config.update_repos()
  end

  defp register() do
    Hex.Shell.info(
      "By registering an account on Hex.pm you accept all our " <>
        "policies and terms of service found at https://hex.pm/policies\n"
    )

    username = Hex.Shell.prompt("Username:") |> Hex.string_trim()
    email = Hex.Shell.prompt("Email:") |> Hex.string_trim()
    password = Mix.Tasks.Hex.password_get("Account password:") |> Hex.string_trim()
    confirm = Mix.Tasks.Hex.password_get("Account password (confirm):") |> Hex.string_trim()

    if password != confirm do
      Mix.raise("Entered passwords do not match")
    end

    Hex.Shell.info("Registering...")
    create_user(username, email, password)
  end

  defp create_user(username, email, password) do
    case Hex.API.User.new(username, email, password) do
      {:ok, {code, _, _}} when code in 200..299 ->
        Mix.Tasks.Hex.generate_all_api_keys(username, password)

        Hex.Shell.info(
          "You are required to confirm your email to access your account, " <>
            "a confirmation email has been sent to #{email}"
        )

      other ->
        Hex.Shell.error("Registration of user #{username} failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp auth(opts) do
    Mix.Tasks.Hex.auth(opts)
  end

  defp revoke_all_keys() do
    auth = Mix.Tasks.Hex.auth_info(:write)

    Hex.Shell.info("Revoking all keys...")

    case Hex.API.Key.delete_all(auth) do
      {:ok, {code, %{"name" => _, "authing_key" => true}, _headers}} when code in 200..299 ->
        Mix.Tasks.Hex.User.run(["deauth"])

      other ->
        Hex.Shell.error("Key revocation failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp revoke_key(key) do
    auth = Mix.Tasks.Hex.auth_info(:write)

    Hex.Shell.info("Revoking key #{key}...")

    case Hex.API.Key.delete(key, auth) do
      {:ok, {200, %{"name" => ^key, "authing_key" => true}, _headers}} ->
        Mix.Tasks.Hex.User.run(["deauth"])
        :ok

      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Key revocation failed")
        Hex.Utils.print_error_result(other)
    end
  end

  # TODO: print permissions
  defp list_keys() do
    auth = Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.Key.get(auth) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        values =
          Enum.map(body, fn %{"name" => name, "inserted_at" => time} ->
            [name, time]
          end)

        Mix.Tasks.Hex.print_table(["Name", "Created at"], values)

      other ->
        Hex.Shell.error("Key fetching failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp generate_unencrypted_key(opts) do
    username = Hex.Shell.prompt("Username:") |> Hex.string_trim()
    password = Mix.Tasks.Hex.password_get("Account password:") |> Hex.string_trim()
    {:ok, hostname} = :inet.gethostname()
    key_name = "#{opts[:key_name] || hostname}-api"
    Hex.Shell.info("Generating API keys...")

    result =
      Mix.Tasks.Hex.generate_api_key(
        key_name,
        [%{"domain" => "api"}],
        user: username,
        pass: password
      )

    case result do
      {:ok, secret} -> Hex.Shell.info(secret)
      :error -> :ok
    end
  end
end
