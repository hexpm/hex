defmodule Mix.Tasks.Hex.User do
  use Mix.Task

  @shortdoc "Manages your Hex user account"

  @moduledoc """
  Hex user tasks.

  ## Print the current user

      $ mix hex.user whoami

  ## Authorize a new user

  Authorizes a new user on the local machine using OAuth device flow.

      $ mix hex.user auth

  ## Deauthorize the user

  Deauthorizes the user from the local machine by removing OAuth tokens from the Hex config.

      $ mix hex.user deauth
  """
  @behaviour Hex.Mix.TaskDescription

  @switches []

  @impl true
  def run(args) do
    Hex.start()
    {_opts, args} = OptionParser.parse!(args, strict: @switches)

    case args do
      ["whoami"] ->
        whoami()

      ["auth"] ->
        auth()

      ["deauth"] ->
        deauth()

      _ ->
        invalid_args()
    end
  end

  @impl true
  def tasks() do
    [
      {"whoami", "Prints the current user"},
      {"auth", "Authorize using OAuth device flow"},
      {"deauth", "Deauthorize the user"}
    ]
  end

  defp invalid_args() do
    Mix.raise("""
    Invalid arguments, expected one of:

    mix hex.user whoami
    mix hex.user auth
    mix hex.user deauth
    """)
  end

  defp whoami() do
    auth = Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.User.me(auth) do
      {:ok, {code, _, body}} when code in 200..299 ->
        Hex.Shell.info(body["username"])

      other ->
        Hex.Shell.error("Failed to auth")
        Hex.Utils.print_error_result(other)
    end
  end

  defp auth() do
    Mix.Tasks.Hex.auth()
  end

  defp deauth() do
    # Clear OAuth tokens
    Mix.Tasks.Hex.clear_oauth_tokens()
    deauth_organizations()

    Hex.Shell.info(
      "Authentication credentials removed from the local machine. " <>
        "To authenticate again, run `mix hex.user auth`"
    )
  end

  defp deauth_organizations() do
    Hex.State.fetch!(:repos)
    |> Enum.reject(fn {name, _config} -> String.starts_with?(name, "hexpm:") end)
    |> Map.new()
    |> Hex.Config.update_repos()
  end
end
