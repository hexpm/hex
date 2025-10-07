defmodule Hex.API.Client do
  @moduledoc false

  def config(opts \\ []) do
    config = %{
      :mix_hex_core.default_config()
      | http_adapter: {Hex.HTTP, %{}},
        api_url: Hex.State.fetch!(:api_url),
        http_user_agent_fragment: user_agent_fragment()
    }

    config
    |> maybe_put_api_key(opts)
    |> maybe_put_otp(opts)
    |> maybe_put_organization(opts)
    |> maybe_put_repository(opts)
  end

  defp maybe_put_api_key(config, opts) do
    cond do
      opts[:key] ->
        # Add Bearer prefix only for OAuth tokens
        token = if opts[:oauth], do: "Bearer #{opts[:key]}", else: opts[:key]
        Map.put(config, :api_key, token)

      opts[:user] && opts[:pass] ->
        # For basic auth, add it as an HTTP header
        base64 = Base.encode64("#{opts[:user]}:#{opts[:pass]}")
        headers = Map.get(config, :http_headers, %{})
        headers = Map.put(headers, "authorization", "Basic #{base64}")
        Map.put(config, :http_headers, headers)

      true ->
        config
    end
  end

  defp maybe_put_otp(config, opts) do
    if otp = opts[:otp] do
      Map.put(config, :api_otp, otp)
    else
      config
    end
  end

  defp maybe_put_organization(config, opts) do
    if org = opts[:organization] || opts[:api_organization] do
      Map.put(config, :api_organization, to_string(org))
    else
      config
    end
  end

  defp maybe_put_repository(config, opts) do
    if repo = opts[:repository] do
      Map.put(config, :api_repository, repo)
    else
      config
    end
  end

  def build_config(repo, auth) do
    opts = if repo, do: [repository: to_string(repo)], else: []
    opts = if auth, do: Keyword.merge(opts, auth), else: opts
    config(opts)
  end

  def user_agent_fragment do
    ci = if Hex.State.fetch!(:ci), do: " (CI)", else: ""
    "hex/#{Hex.version()} (elixir/#{System.version()})#{ci}"
  end
end
