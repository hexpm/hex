defmodule Hex.Application do
  @moduledoc false

  use Application

  def start(_, _) do
    dev_setup()

    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)

    start_httpc()

    opts = [strategy: :one_for_one, name: Hex.Supervisor]
    Supervisor.start_link(children(), opts)
  end

  def stop(_state) do
    Mix.RemoteConverger.register(nil)

    if function_exported?(Mix.SCM, :delete, 1) do
      apply(Mix.SCM, :delete, [Hex.SCM])
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
        Hex.RepoIdentifier,
        Hex.State,
        Hex.Server,
        {Hex.Parallel, [:hex_fetcher]}
      ]
    end
  else
    defp children do
      [
        Hex.Netrc.Cache,
        Hex.RepoIdentifier,
        Hex.State,
        Hex.Server,
        {Hex.Parallel, [:hex_fetcher]},
        Hex.Registry.Server,
        Hex.UpdateChecker
      ]
    end
  end
end
