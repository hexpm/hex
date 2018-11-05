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

  def should_warn_lock_version?() do
    GenServer.call(@name, :should_warn_lock_version?)
  end

  def init([]) do
    {:ok, state()}
  end

  def handle_call(:reset, _from, _state) do
    {:reply, :ok, state()}
  end

  def handle_call(:should_warn_lock_version?, _from, %{warned_tuple_size: false} = state) do
    {:reply, true, %{state | warned_tuple_size: true}}
  end

  def handle_call(:should_warn_lock_version?, _from, %{warned_tuple_size: true} = state) do
    {:reply, false, state}
  end

  defp state() do
    %{warned_tuple_size: false}
  end
end
