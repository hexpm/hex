defmodule Hex.UpdateChecker do
  use GenServer

  @name __MODULE__
  @timeout 60_000
  @update_interval 24 * 60 * 60

  def start_link(opts \\ []) do
    opts = Keyword.put_new(opts, :name, @name)
    GenServer.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    {:ok, state()}
  end

  def start_check() do
    GenServer.cast(@name, :start_check)
  end

  def check() do
    GenServer.call(@name, :check, @timeout)
    |> print_update_message()
  end

  def handle_cast(:start_check, state) do
    if not state.started and not Hex.State.fetch!(:offline?) and check_update?() do
      Task.async(fn ->
        {:installs, Hex.Repo.get_installs()}
      end)
      {:noreply, %{state | started: true}}
    else
      {:noreply, %{state | started: true, done: true}}
    end
  end

  def handle_call(:check, _from, %{started: true, done: true} = state) do
    {:reply, :already_checked, state}
  end
  def handle_call(:check, from, %{started: true, reply: nil} = state) do
    {:noreply, %{state | from: from}}
  end
  def handle_call(:check, from, %{started: true} = state) do
    {:reply, state.reply, %{state | from: from, done: true}}
  end
  def handle_call(:check, _from, %{started: false} = state) do
    {:reply, :latest, state}
  end

  def handle_info({_ref, {:installs, result}}, state) do
    result =
      case result do
        {:ok, {code, body, _headers}} when code in 200..299 ->
          Hex.Repo.find_new_version_from_csv(body)
        other ->
          Hex.Shell.error("Failed to check for new Hex version")
          Hex.Utils.print_error_result(other)
          # Treat failure as latest
          :latest
      end

    Hex.Registry.Server.last_update(:calendar.universal_time())
    state = reply(result, state)
    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, _pid, :normal}, state) do
    {:noreply, state}
  end

  defp print_update_message(:already_checked), do: :ok
  defp print_update_message(:latest), do: :ok
  defp print_update_message({:http_error, reason}) do
    Hex.Shell.error "Hex update check failed, HTTP ERROR: #{inspect reason}"
    :ok
  end
  defp print_update_message({:status, status}) do
    Hex.Shell.error "Hex update check failed, status code: #{status}"
    :ok
  end
  defp print_update_message({:version, version}) do
    Hex.Shell.warn "A new Hex version is available (#{Hex.version} < #{version}), " <>
                   "please update with `mix local.hex`"
    :ok
  end

  defp reply(reply, %{from: nil} = state) do
    %{state | reply: reply}
  end
  defp reply(reply, %{from: from} = state) do
    GenServer.reply(from, reply)
    %{state | from: nil, done: true}
  end

  defp check_update?() do
    if last = Hex.Registry.Server.last_update() do
      now = :calendar.universal_time() |> :calendar.datetime_to_gregorian_seconds()
      last = :calendar.datetime_to_gregorian_seconds(last)

      now - last > @update_interval
    else
      true
    end
  end

  defp state() do
    %{from: nil,
      reply: nil,
      done: false,
      started: false}
  end
end
