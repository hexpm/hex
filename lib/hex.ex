defmodule Hex do
  @default_url "https://hex.pm"
  @default_cdn "https://s3.amazonaws.com/s3.hex.pm"
  @default_home "~/.hex"

  @logged_keys ["http_proxy", "HTTP_PROXY", "https_proxy", "HTTPS_PROXY"]

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

    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)

    :inets.start(:httpc, profile: :hex)
    http_opts()

    children = [
      worker(Hex.State, []),
      worker(Hex.Registry, []),
      worker(Hex.Parallel, [:hex_fetcher, [max_parallel: 8]]),
    ]

    opts = [strategy: :one_for_one, name: Hex.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def home do
    Application.get_env(:hex, :home) || Path.expand(@default_home)
  end

  def home(home) do
    home = Path.expand(home)
    Application.put_env(:hex, :home, home)
  end

  def version,        do: unquote(Mix.Project.config[:version])
  def elixir_version, do: unquote(System.version)

  defp http_opts do
    opts = [
      max_sessions: 4,
      max_keep_alive_length: 4,
      keep_alive_timeout: 120_000,
      max_pipeline_length: 4,
      pipeline_timeout: 60_000,
    ]

    :httpc.set_options(opts, :hex)
  end
end
