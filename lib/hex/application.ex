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
        Hex.State,
        Hex.HTTP.Pool,
        Hex.Server,
        {Hex.Parallel, [:hex_registry_fetcher, &registry_concurrency/0]},
        {Hex.Parallel, [:hex_tarball_fetcher, &tarball_concurrency/0]}
      ]
    end
  else
    defp children do
      [
        Hex.Netrc.Cache,
        Hex.State,
        Hex.HTTP.Pool,
        Hex.Server,
        {Hex.Parallel, [:hex_registry_fetcher, &registry_concurrency/0]},
        {Hex.Parallel, [:hex_tarball_fetcher, &tarball_concurrency/0]},
        Hex.Registry.Server,
        Hex.UpdateChecker
      ]
    end
  end

  # Registry and policy files are a few kilobytes each, so more of them are
  # fetched at a time than package tarballs
  defp registry_concurrency, do: 4 * Hex.State.fetch!(:http_concurrency)

  defp tarball_concurrency, do: Hex.State.fetch!(:http_concurrency)
end
