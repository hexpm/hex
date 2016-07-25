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
    dev_setup()

    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)

    Hex.Version.start
    start_httpc()

    children = [
      worker(Hex.State, []),
      worker(Hex.Registry.Server, []),
      worker(Hex.Parallel, [:hex_fetcher, [max_parallel: 64]]),
    ]

    opts = [strategy: :one_for_one, name: Hex.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def version,        do: unquote(Mix.Project.config[:version])
  def elixir_version, do: unquote(System.version)
  def otp_version,    do: unquote(Hex.Utils.otp_version)

  defp start_httpc() do
    :inets.start(:httpc, profile: :hex)
    opts = [
      max_sessions: 8,
      max_keep_alive_length: 4,
      max_pipeline_length: 4,
      keep_alive_timeout: 120_000,
      pipeline_timeout: 60_000
    ]
    :httpc.set_options(opts, :hex)
  end

  if Version.compare(System.version, "1.3.0") == :lt do
    def string_to_charlist(string), do: String.to_char_list(string)
  else
    def string_to_charlist(string), do: String.to_charlist(string)
  end

  if Mix.env in [:dev, :test] do
    defp dev_setup do
      :erlang.system_flag(:backtrace_depth, 30)
    end
  else
    defp dev_setup, do: :ok
  end
end
