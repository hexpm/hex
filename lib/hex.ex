defmodule Hex do
  use Application

  def start do
    {:ok, _} = Application.ensure_all_started(:hex)
  end

  def stop do
    case Application.stop(:hex) do
      :ok -> :ok
      {:error, {:not_started, :hex}} -> :ok
    end
  end

  def start(_, _) do
    import Supervisor.Spec

    Mix.RemoteConverger.register(Hex.RemoteConverger)

    Hex.Version.start
    start_httpc()

    children = [
      worker(Hex.State, []),
      worker(Hex.Parallel, [:hex_fetcher, [max_parallel: 64]]),
      supervisor(Hex.RegistrySupervisor, []),
    ]

    opts = [strategy: :one_for_one, name: Hex.Supervisor]
    supervisor = Supervisor.start_link(children, opts)

    Mix.Project.config
    |> Keyword.get(:hex_plugins, [])
    |> Enum.concat([Hex.Plugin])
    |> Enum.each(&apply(&1, :init, []))

    supervisor
  end

  def version,        do: unquote(Mix.Project.config[:version])
  def elixir_version, do: unquote(System.version)
  def otp_version,    do: unquote(Hex.Utils.otp_version)

  defp start_httpc() do
    :inets.start(:httpc, profile: :hex)
    opts = [
      max_sessions: 8,
      max_keep_alive_length: 4,
      keep_alive_timeout: 120_000
    ]
    :httpc.set_options(opts, :hex)
  end
end
