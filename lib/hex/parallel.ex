defmodule Hex.Parallel do
  use GenServer

  def start_link(name, opts) do
    GenServer.start_link(__MODULE__, new_state(opts), name: name)
  end

  def run(name, id, fun) do
    GenServer.cast(name, {:run, id, fun})
  end

  def await(name, id, timeout \\ :infinity) do
    GenServer.call(name, {:await, id}, timeout)
  end

  def handle_cast({:run, id, fun}, state) do
    state = run_task(id, fun, state)
    {:noreply, state}
  end

  def handle_call({:await, id}, from, state) do
    if result = state.finished[id] do
      state = update_in(state.finished, &Map.delete(&1, id))
      {:reply, result, state}
    else
      state = update_in(state.waiting_reply, &Map.put(&1, id, from))
      {:noreply, state}
    end
  end

  def handle_info(message, state) do
    tasks = Map.keys(state.running)

    case Task.find(tasks, message) do
      {result, task} ->
        id = state.running[task]
        state = update_in(state.running, &Map.delete(&1, task))

        if from = state.waiting_reply[id] do
          GenServer.reply(from, result)
          state = update_in(state.waiting_reply, &Map.delete(&1, id))
        else
          state = update_in(state.finished, &Map.put(&1, id, result))
        end

        case :queue.out(state.waiting) do
          {{:value, {id, fun}}, waiting} ->
            state = put_in(state.waiting, waiting)
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
      state = update_in(state.waiting, &:queue.in({id, fun}, &1))
    else
      task = Task.async(fun)
      state = update_in(state.running, &Map.put(&1, task, id))
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
