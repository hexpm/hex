defmodule Hex do
  @moduledoc false

  def start() do
    {:ok, _} = Application.ensure_all_started(:hex)
  end

  def stop() do
    case Application.stop(:hex) do
      :ok -> :ok
      {:error, {:not_started, :hex}} -> :ok
    end
  end

  # For compatibility during development
  def start(start_type, start_args) do
    Hex.Application.start(start_type, start_args)
  end

  def version(), do: unquote(Mix.Project.config()[:version])
  def elixir_version(), do: unquote(System.version())
  def otp_version(), do: unquote(Hex.Utils.otp_version())
end
