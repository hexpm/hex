defmodule Hex.Application do
  @moduledoc false

  use Application

  def start(_, _) do
    dev_setup()

    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)

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

  if Mix.env() == :test do
    defp children do
      [
        Hex.Netrc.Cache,
        Hex.OAuth,
        Hex.Repo,
        Hex.State,
        Hex.HTTP.Pool,
        Hex.Server,
        {Hex.Parallel, [:hex_fetcher]}
      ]
    end
  else
    defp children do
      [
        Hex.Netrc.Cache,
        Hex.OAuth,
        Hex.Repo,
        Hex.State,
        Hex.HTTP.Pool,
        Hex.Server,
        {Hex.Parallel, [:hex_fetcher]},
        Hex.Registry.Server,
        Hex.UpdateChecker
      ]
    end
  end
end
