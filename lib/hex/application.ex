defmodule Hex.Application do
  @moduledoc false

  use Application

  def start(_, _) do
    dev_setup()

    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)

    warn_ssl()
    start_httpc()

    opts = [strategy: :one_for_one, name: Hex.Supervisor]
    Supervisor.start_link(children(), opts)
  end

  def stop(_state) do
    Mix.RemoteConverger.register(nil)

    if function_exported?(Mix.SCM, :delete, 1) do
      Mix.SCM.delete(Hex.SCM)
    end

    :ok
  end

  if Mix.env() in [:dev, :test] do
    defp dev_setup do
      :erlang.system_flag(:backtrace_depth, 20)
    end
  else
    defp dev_setup, do: :ok
  end

  defp warn_ssl() do
    case Application.load(:ssl) do
      :ok ->
        if :application.get_key(:ssl, :vsn) == {:ok, ~c"10.2"} do
          Hex.Shell.warn("""
          You are using an OTP release with the application ssl-10.2 which has a vulnerability \
          making it susceptible to man-in-the-middle attacks. You are strongly recommended to \
          upgrade to newer version, ssl-10.2.1+ or OTP-23.2.2+.
          """)
        end

      {:error, _} ->
        :ok
    end
  end

  defp start_httpc() do
    :inets.start(:httpc, profile: :hex)

    opts = [
      max_sessions: 8,
      max_keep_alive_length: 4,
      keep_alive_timeout: 120_000
    ]

    :httpc.set_options(opts, :hex)
  end

  if Mix.env() == :test do
    defp children do
      [
        Hex.Netrc.Cache,
        Hex.State,
        Hex.Server,
        {Hex.Parallel, [:hex_fetcher]}
      ]
    end
  else
    defp children do
      [
        Hex.Netrc.Cache,
        Hex.State,
        Hex.Server,
        {Hex.Parallel, [:hex_fetcher]},
        Hex.Registry.Server,
        Hex.UpdateChecker
      ]
    end
  end
end
