defmodule Hex.HTTP.Pool.HTTP1.Worker do
  @moduledoc false

  # A GenServer that owns a single Mint HTTP/1 connection.
  # It handles exactly one in-flight request at a time.
  #
  # The worker is started with an already-established connection (handed in by
  # the pool, which may have probed ALPN) or it establishes the connection
  # lazily on the first request.

  use GenServer

  alias Hex.Mint.HTTP

  @idle_timeout 30_000

  def start_link({key, connect_fun}) do
    GenServer.start_link(__MODULE__, {key, connect_fun})
  end

  def start_link({key, connect_fun, conn}) do
    GenServer.start_link(__MODULE__, {key, connect_fun, conn})
  end

  def request(pid, method, path, headers, body, timeout) do
    GenServer.call(pid, {:request, method, path, headers, body}, timeout)
  catch
    :exit, {:timeout, _} ->
      {:error, :timeout}

    :exit, {reason, _} ->
      {:error, reason}
  end

  @impl true
  def init({key, connect_fun}) do
    state = %{
      key: key,
      connect_fun: connect_fun,
      conn: nil,
      request_ref: nil,
      from: nil,
      status: nil,
      headers: [],
      data: [],
      idle_timer: schedule_idle_timeout()
    }

    {:ok, state}
  end

  @impl true
  def init({key, connect_fun, conn}) do
    # The connection was opened in another process (the probe caller);
    # take ownership so socket messages are delivered to us.
    {:ok, conn} = HTTP.controlling_process(conn, self())
    {:ok, conn} = HTTP.set_mode(conn, :active)

    state = %{
      key: key,
      connect_fun: connect_fun,
      conn: conn,
      request_ref: nil,
      from: nil,
      status: nil,
      headers: [],
      data: [],
      idle_timer: schedule_idle_timeout()
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:request, method, path, headers, body}, from, state) do
    state = cancel_idle_timer(state)

    case ensure_connected(state) do
      {:ok, state} ->
        case HTTP.request(state.conn, method, path, headers, body) do
          {:ok, conn, ref} ->
            state = %{
              state
              | conn: conn,
                request_ref: ref,
                from: from,
                status: nil,
                headers: [],
                data: []
            }

            {:noreply, state}

          {:error, conn, reason} ->
            _ = safe_close(conn)
            {:stop, :normal, {:error, reason}, %{state | conn: nil}}
        end

      {:error, reason} ->
        {:stop, :normal, {:error, reason}, state}
    end
  end

  @impl true
  def handle_info(:idle_timeout, %{from: nil} = state) do
    _ = safe_close(state.conn)
    {:stop, :normal, state}
  end

  def handle_info(:idle_timeout, state) do
    # Busy - ignore and reschedule
    {:noreply, %{state | idle_timer: schedule_idle_timeout()}}
  end

  def handle_info(message, %{conn: conn} = state) when conn != nil do
    case HTTP.stream(conn, message) do
      {:ok, conn, responses} ->
        state = %{state | conn: conn}
        state = Enum.reduce(responses, state, &process_response/2)
        {:noreply, state}

      {:error, conn, reason, responses} ->
        state = %{state | conn: conn}
        state = Enum.reduce(responses, state, &process_response/2)

        state =
          if state.from do
            GenServer.reply(state.from, {:error, reason})
            reset_request(state)
          else
            state
          end

        _ = safe_close(state.conn)
        {:stop, :normal, %{state | conn: nil}}

      :unknown ->
        {:noreply, state}
    end
  end

  def handle_info(_message, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    if state.from, do: GenServer.reply(state.from, {:error, :pool_worker_terminated})
    if state.conn, do: safe_close(state.conn)
    :ok
  end

  ## Helpers

  defp ensure_connected(%{conn: nil} = state) do
    case state.connect_fun.() do
      {:ok, conn} ->
        {:ok, conn} = HTTP.set_mode(conn, :active)
        {:ok, %{state | conn: conn}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp ensure_connected(%{conn: conn} = state) do
    if HTTP.open?(conn, :write) do
      {:ok, state}
    else
      _ = safe_close(conn)
      ensure_connected(%{state | conn: nil})
    end
  end

  defp process_response({:status, ref, status}, %{request_ref: ref} = state) do
    %{state | status: status}
  end

  defp process_response({:headers, ref, headers}, %{request_ref: ref} = state) do
    %{state | headers: state.headers ++ headers}
  end

  defp process_response({:data, ref, chunk}, %{request_ref: ref} = state) do
    %{state | data: [state.data | chunk]}
  end

  defp process_response({:done, ref}, %{request_ref: ref} = state) do
    body = IO.iodata_to_binary(state.data)
    GenServer.reply(state.from, {:ok, state.status, state.headers, body})
    reset_request(state)
  end

  defp process_response({:error, ref, reason}, %{request_ref: ref} = state) do
    GenServer.reply(state.from, {:error, reason})
    reset_request(state)
  end

  defp process_response(_other, state), do: state

  defp reset_request(state) do
    %{
      state
      | request_ref: nil,
        from: nil,
        status: nil,
        headers: [],
        data: [],
        idle_timer: schedule_idle_timeout()
    }
  end

  defp schedule_idle_timeout do
    Process.send_after(self(), :idle_timeout, @idle_timeout)
  end

  defp cancel_idle_timer(%{idle_timer: nil} = state), do: state

  defp cancel_idle_timer(%{idle_timer: ref} = state) do
    _ = Process.cancel_timer(ref)
    %{state | idle_timer: nil}
  end

  defp safe_close(nil), do: :ok

  defp safe_close(conn) do
    try do
      HTTP.close(conn)
    catch
      _, _ -> :ok
    end
  end
end
