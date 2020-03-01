defmodule Hex.Application do
  @moduledoc false

  use Application

  def start(_, _) do
    dev_setup()

    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)

    Hex.Version.start()
    start_httpc()

    opts = [strategy: :one_for_one, name: Hex.Supervisor]
    Supervisor.start_link(children(), opts)
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

  defmacrop worker(module) do
    if Version.compare(System.version(), "1.5.0") == :lt do
      import Supervisor.Spec
      quote do: worker(unquote(module), [[]])
    else
      quote do: unquote(module)
    end
  end

  defmacrop worker(module, args) do
    if Version.compare(System.version(), "1.5.0") == :lt do
      import Supervisor.Spec
      quote do: worker(unquote(module), [unquote(args)])
    else
      quote do: {unquote(module), unquote(args)}
    end
  end

  if Mix.env() == :test do
    defp children do
      [
        worker(Hex.State),
        worker(Hex.Server),
        worker(Hex.Parallel, [:hex_fetcher])
      ]
    end
  else
    defp children do
      [
        worker(Hex.State),
        worker(Hex.Server),
        worker(Hex.Parallel, [:hex_fetcher]),
        worker(Hex.Registry.Server),
        worker(Hex.UpdateChecker)
      ]
    end
  end
end
