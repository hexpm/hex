defmodule Hex.Parallel do
  @moduledoc """
  Runs a number of jobs (with an upper bound) in parallel and
  awaits them to finish.
  """

  use GenServer

  def start_link(name, opts) do
    GenServer.start_link(__MODULE__, new_state(opts), name: name)
  end

  @doc """
  """
  @spec run(GenServer.server, any, (() -> any)) :: :ok
  def run(name, id, fun) do
    GenServer.cast(name, {:run, id, fun})
  end

  @doc """
  """
  @spec await(GenServer.server, any, timeout) :: any
  def await(name, id, timeout) do
    GenServer.call(name, {:await, id}, timeout)
  end

  def clear(name) do
    GenServer.call(name, :clear)
  end

  def handle_cast({:run, id, fun}, state) do
    state = run_task(id, fun, state)
    {:noreply, state}
  end

  def handle_call({:await, id}, from, state) do
    if result = state.finished[id] do
      state = %{state | finished: Map.delete(state.finished, id)}
      {:reply, result, state}
    else
      state = %{state | waiting_reply: Map.put(state.waiting_reply, id, from)}
      {:noreply, state}
    end
  end

  def handle_call(:clear, _from, state) do
    Enum.each(state.running, fn {%Task{pid: pid}, _} ->
      Process.exit(pid, :stop)
    end)

    state = %{state | running: %{}, finished: %{}, waiting: :queue.new, waiting_reply: %{}}
    {:reply, :ok, state}
  end

  def handle_info(message, state) do
    tasks = Map.keys(state.running)

    case Task.find(tasks, message) do
      {result, task} ->
        id = state.running[task]
        state = %{state | running: Map.delete(state.running, task)}

        if from = state.waiting_reply[id] do
          GenServer.reply(from, result)
          state = %{state | waiting_reply: Map.delete(state.waiting_reply, id)}
        else
          state = %{state | finished: Map.put(state.finished, id, result)}
        end

        case :queue.out(state.waiting) do
          {{:value, {id, fun}}, waiting} ->
            state = %{state | waiting: waiting}
            state = run_task(id, fun, state)
          {:empty, _} ->
            :ok
        end

        {:noreply, state}
      nil ->
        {:noreply, state}
    end
  end

  defp run_task(id, fun, state) do
    if Map.size(state.running) >= state.max_jobs do
      state = %{state | waiting: :queue.in({id, fun}, state.waiting)}
    else
      task = Task.async(fun)
      state = %{state | running: Map.put(state.running, task, id)}
    end

    state
  end

  defp new_state(opts) do
    %{max_jobs: opts[:max_parallel] || 4,
      running: %{},
      finished: %{},
      waiting: :queue.new,
      waiting_reply: %{}}
  end
end
