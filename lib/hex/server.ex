defmodule Hex.Server do
  @moduledoc false

  use GenServer

  @name __MODULE__

  def start_link(opts \\ []) do
    opts = Keyword.put_new(opts, :name, @name)
    GenServer.start_link(__MODULE__, [], opts)
  end

  def reset() do
    GenServer.call(@name, :reset)
  end

  def should_warn_lock_version?(name \\ @name) do
    GenServer.call(name, :should_warn_lock_version?)
  end

  def should_warn_registry_version?(name \\ @name) do
    GenServer.call(name, :should_warn_registry_version?)
  end

  def init([]) do
    {:ok, state()}
  end

  def handle_call(:reset, _from, _state) do
    {:reply, :ok, state()}
  end

  def handle_call(:should_warn_lock_version?, _from, %{warned_lock_version: false} = state) do
    {:reply, true, %{state | warned_lock_version: true}}
  end

  def handle_call(:should_warn_lock_version?, _from, %{warned_lock_version: true} = state) do
    {:reply, false, state}
  end

  def handle_call(:should_warn_registry_version?, _from, %{warned_registry_version: false} = state) do
    {:reply, true, %{state | warned_registry_version: true}}
  end

  def handle_call(:should_warn_registry_version?, _from, %{warned_registry_version: true} = state) do
    {:reply, false, state}
  end

  defp state() do
    %{
      warned_lock_version: false,
      warned_registry_version: false
    }
  end
end
