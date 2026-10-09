defmodule Hex.WorkloadIdentity do
  @moduledoc false

  alias Hex.API.Client

  @doc """
  Exchanges the CI job's OIDC token for API auth scoped to one package.

  Returns no auth outside a supported CI job or when the user configured
  credentials, since those take precedence.
  """
  def auth!(repository, package) do
    scope = "package:#{repository}/#{package}"

    case :mix_hex_cli_auth.workload_identity_auth(Client.config(), scope) do
      {:ok, "Bearer " <> token} ->
        Hex.Shell.info("Authenticated with Workload Identity")
        [key: token, oauth: true]

      :none ->
        []

      {:error, reason} ->
        Mix.raise(error_message(reason, scope))
    end
  end

  @doc """
  Whether the job can authenticate with a workload identity.
  """
  def available? do
    :mix_hex_oidc.detect_provider() != :none
  end

  @doc """
  Exchanges the CI job's OIDC token for each organization's repository token.

  The fetches from the repository reuse the token, or the failure. A failure
  is reported by each fetch the repository refuses without credentials.
  """
  def repository_auth(organizations) do
    Enum.each(organizations, fn organization ->
      case Hex.Repo.resolve_auth("hexpm:#{organization}") do
        {:ok, _repo_key, _auth_context} ->
          Hex.Shell.info(
            "Authenticated to the #{organization} organization with Workload Identity"
          )

        _other ->
          :ok
      end
    end)
  end

  @doc """
  The message for a failed exchange for an organization's repository.
  """
  def repository_error_message("hexpm:" <> organization, reason) do
    error_message(reason, "repository:#{organization}") <>
      ". To fetch without Workload Identity, authenticate with " <>
      "`mix hex.organization auth #{organization} --key KEY`"
  end

  defp error_message(reason, scope) do
    "Workload Identity authentication failed, " <> describe(reason, scope)
  end

  defp describe({:oidc_audience_failed, response}, _scope) do
    "could not fetch the OIDC audience from Hex: " <> describe_response(response)
  end

  defp describe({:token_exchange_failed, response}, scope) do
    "Hex did not grant a token for #{scope}: " <> describe_response(response)
  end

  defp describe({:oidc_token_request_failed, status}, _scope) do
    "GitHub Actions refused to issue an OIDC token (HTTP #{status})"
  end

  defp describe({:oidc_token_unavailable, reason}, _scope) do
    "could not request an OIDC token from GitHub Actions: " <> inspect(reason)
  end

  defp describe(:oidc_token_missing, _scope) do
    "GitHub Actions answered without an OIDC token"
  end

  defp describe(:json_unavailable, _scope) do
    "reading the OIDC token requires Erlang/OTP 27 or later"
  end

  defp describe_response({:ok, {status, _headers, %{"error" => error} = body}}) do
    case body["error_description"] do
      nil -> "#{error} (HTTP #{status})"
      description -> "#{error}: #{description} (HTTP #{status})"
    end
  end

  defp describe_response({:ok, {status, _headers, %{"message" => message}}}) do
    "#{message} (HTTP #{status})"
  end

  defp describe_response({:ok, {status, _headers, _body}}), do: "HTTP #{status}"
  defp describe_response({:error, reason}), do: inspect(reason)
end
