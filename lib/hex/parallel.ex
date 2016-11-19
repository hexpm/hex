defmodule Hex.Parallel do
  @moduledoc """
  Runs a number of jobs (with an upper bound) in parallel and
  awaits them to finish.
  """

  use GenServer

  def start_link(name) do
    GenServer.start_link(__MODULE__, [], name: name)
  end

  @spec run(GenServer.server, any, Keyword.t, (() -> any)) :: :ok
  def run(name, id, opts \\ [], fun) do
    GenServer.call(name, {:run, id, opts, fun})
  end

  @spec await(GenServer.server, any, timeout) :: any
  def await(name, id, timeout) do
    GenServer.call(name, {:await, id}, timeout)
  end

  def clear(name) do
    GenServer.call(name, :clear)
  end

  def init([]) do
    {:ok, new_state()}
  end

  def handle_call({:run, id, opts, fun}, {pid, _ref}, state) do
    await? = Keyword.get(opts, :await, true)
    state = run_task(id, fun, state)

    state =
      if await? do
        state
      else
        %{state | waiting_reply: Map.put(state.waiting_reply, id, {:send, pid})}
      end

    {:reply, :ok, state}
  end

  def handle_call({:await, id}, from, state) do
    if result = state.finished[id] do
      state = %{state | finished: Map.delete(state.finished, id)}
      {:reply, result, state}
    else
      state = %{state | waiting_reply: Map.put(state.waiting_reply, id, {:gen, from})}
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

  def handle_info({ref, message}, state) when is_reference(ref) do
    tasks = Map.keys(state.running)

    if task = Enum.find(tasks, &(&1.ref == ref)) do
      id = state.running[task]
      state =
        %{state | running: Map.delete(state.running, task)}
        |> reply(id, message)
        |> next_task

      {:noreply, state}
    else
      {:noreply, state}
    end
  end

  def handle_info({:DOWN, ref, _, proc, reason}, state) do
    tasks = Map.keys(state.running)

    if Enum.find(tasks, &(&1.ref == ref)) do
      {:stop, {proc, reason}, state}
    else
      {:noreply, state}
    end
  end

  defp reply(state, id, message) do
    case state.waiting_reply[id] do
      {:gen, from} ->
        GenServer.reply(from, message)
        %{state | waiting_reply: Map.delete(state.waiting_reply, id)}
      {:send, pid} ->
        send(pid, message)
        %{state | waiting_reply: Map.delete(state.waiting_reply, id)}
      nil ->
        %{state | finished: Map.put(state.finished, id, message)}
    end
  end

  defp next_task(state) do
    case :queue.out(state.waiting) do
      {{:value, {id, fun}}, waiting} ->
        state = %{state | waiting: waiting}
        run_task(id, fun, state)
      {:empty, _} ->
        state
    end
  end

  defp run_task(id, fun, state) do
    if Map.size(state.running) >= state.max_jobs do
      %{state | waiting: :queue.in({id, fun}, state.waiting)}
    else
      task = Task.async(fun)
      %{state | running: Map.put(state.running, task, id)}
    end
  end

  defp new_state() do
    %{max_jobs: Hex.State.fetch!(:http_concurrency),
      running: %{},
      finished: %{},
      waiting: :queue.new,
      waiting_reply: %{}}
  end
end
