defmodule Hex.HTTP.Pool.HTTP2 do
  @moduledoc false

  # HTTP/2 pool: a single `:gen_statem` process owning one multiplexed
  # Mint HTTP/2 connection per {scheme, host, port}.
  #
  # States:
  #   * :disconnected     - no active connection; next request triggers connect
  #   * :connecting       - connection being established
  #   * :connected        - accepting new requests, multiplexing on one conn
  #   * :connected_read_only - server sent GOAWAY; finish in-flight, reject new
  #
  # Requests are mapped 1:1 to Mint stream refs. Responses arriving out of
  # order (HTTP/2 multiplexing) are attributed to their `from` by ref.

  @behaviour :gen_statem

  alias Hex.Mint.HTTP

  @connect_timeout 15_000

  @impl :gen_statem
  def callback_mode, do: [:state_functions, :state_enter]

  def start_link({key, connect_fun, opts}) do
    case Keyword.fetch(opts, :name) do
      {:ok, name} -> :gen_statem.start_link(name, __MODULE__, {key, connect_fun, opts}, [])
      :error -> :gen_statem.start_link(__MODULE__, {key, connect_fun, opts}, [])
    end
  end

  def request(pid, method, path, headers, body, timeout) do
    :gen_statem.call(pid, {:request, method, path, headers, body}, timeout)
  catch
    :exit, {:timeout, _} -> {:error, :timeout}
    :exit, {reason, _} -> {:error, reason}
  end

  @impl :gen_statem
  def init({key, connect_fun, opts}) do
    data = %{
      key: key,
      connect_fun: connect_fun,
      conn: Keyword.get(opts, :initial_conn),
      requests: %{},
      backoff: 0
    }

    # If we were seeded with an existing connection, jump straight to :connected
    if data.conn do
      case HTTP.set_mode(data.conn, :active) do
        {:ok, conn} -> {:ok, :connected, %{data | conn: conn}}
        {:error, _} -> {:ok, :disconnected, %{data | conn: nil}}
      end
    else
      {:ok, :disconnected, data}
    end
  end

  ## :disconnected

  def disconnected(:enter, _old, data) do
    delay = next_backoff(data.backoff)
    {:keep_state, %{data | backoff: delay}, [{:state_timeout, delay, :reconnect}]}
  end

  def disconnected(:state_timeout, :reconnect, _data) do
    {:next_state, :connecting, :ok}
  end

  def disconnected({:call, from}, {:request, _, _, _, _}, _data) do
    {:keep_state_and_data, [{:reply, from, {:error, :disconnected}}]}
  end

  def disconnected(_event, _content, _data) do
    :keep_state_and_data
  end

  ## :connecting

  def connecting(:enter, _old, data) do
    case data.connect_fun.() do
      {:ok, conn} ->
        case HTTP.set_mode(conn, :active) do
          {:ok, conn} ->
            {:keep_state, %{data | conn: conn, backoff: 0},
             [{:next_event, :internal, :connected}]}

          {:error, _reason} ->
            {:next_state, :disconnected, %{data | conn: nil}}
        end

      {:error, _reason} ->
        {:next_state, :disconnected, %{data | conn: nil}}
    end
  end

  def connecting(:internal, :connected, data) do
    {:next_state, :connected, data}
  end

  def connecting({:call, from}, {:request, _, _, _, _}, _data) do
    # Shouldn't happen in practice since :connecting is a momentary state,
    # but handle it defensively.
    {:keep_state_and_data, [{:reply, from, {:error, :connecting}}]}
  end

  def connecting(_event, _content, _data) do
    :keep_state_and_data
  end

  ## :connected

  def connected(:enter, _old, _data), do: :keep_state_and_data

  def connected({:call, from}, {:request, method, path, headers, body}, data) do
    case HTTP.request(data.conn, method, path, headers, body) do
      {:ok, conn, ref} ->
        req = %{from: from, status: nil, headers: [], data: []}
        data = %{data | conn: conn, requests: Map.put(data.requests, ref, req)}
        {:keep_state, data}

      {:error, conn, reason} ->
        data = %{data | conn: conn}

        if HTTP.open?(conn, :write) do
          {:keep_state, data, [{:reply, from, {:error, reason}}]}
        else
          # Connection broken
          fail_all(data.requests, reason)
          _ = safe_close(conn)
          {:next_state, :disconnected, %{data | conn: nil, requests: %{}},
           [{:reply, from, {:error, reason}}]}
        end
    end
  end

  def connected(:info, message, data) do
    case HTTP.stream(data.conn, message) do
      {:ok, conn, responses} ->
        data = %{data | conn: conn}
        data = Enum.reduce(responses, data, &process_response/2)

        if HTTP.open?(data.conn, :write) do
          {:keep_state, data}
        else
          {:next_state, :connected_read_only, data}
        end

      {:error, conn, reason, responses} ->
        data = %{data | conn: conn}
        data = Enum.reduce(responses, data, &process_response/2)
        fail_all(data.requests, reason)
        _ = safe_close(conn)
        {:next_state, :disconnected, %{data | conn: nil, requests: %{}}}

      :unknown ->
        :keep_state_and_data
    end
  end

  def connected(_event, _content, _data), do: :keep_state_and_data

  ## :connected_read_only

  def connected_read_only(:enter, _old, _data), do: :keep_state_and_data

  def connected_read_only({:call, from}, {:request, _, _, _, _}, _data) do
    # Signal the router to start a fresh pool
    {:keep_state_and_data, [{:reply, from, {:error, :read_only}}]}
  end

  def connected_read_only(:info, message, data) do
    case HTTP.stream(data.conn, message) do
      {:ok, conn, responses} ->
        data = %{data | conn: conn}
        data = Enum.reduce(responses, data, &process_response/2)

        if data.requests == %{} do
          _ = safe_close(conn)
          {:next_state, :disconnected, %{data | conn: nil}}
        else
          {:keep_state, data}
        end

      {:error, conn, reason, responses} ->
        data = %{data | conn: conn}
        data = Enum.reduce(responses, data, &process_response/2)
        fail_all(data.requests, reason)
        _ = safe_close(conn)
        {:next_state, :disconnected, %{data | conn: nil, requests: %{}}}

      :unknown ->
        :keep_state_and_data
    end
  end

  def connected_read_only(_event, _content, _data), do: :keep_state_and_data

  @impl :gen_statem
  def terminate(_reason, _state, data) do
    fail_all(data.requests, :terminated)
    if data.conn, do: safe_close(data.conn)
    :ok
  end

  ## Response handling

  defp process_response({:status, ref, status}, data) do
    update_in(data, [:requests, ref], fn req ->
      req && %{req | status: status}
    end)
  end

  defp process_response({:headers, ref, headers}, data) do
    update_in(data, [:requests, ref], fn req ->
      req && %{req | headers: req.headers ++ headers}
    end)
  end

  defp process_response({:data, ref, chunk}, data) do
    update_in(data, [:requests, ref], fn req ->
      req && %{req | data: [req.data | chunk]}
    end)
  end

  defp process_response({:done, ref}, data) do
    case Map.pop(data.requests, ref) do
      {nil, _} ->
        data

      {req, requests} ->
        body = IO.iodata_to_binary(req.data)
        :gen_statem.reply(req.from, {:ok, req.status, req.headers, body})
        %{data | requests: requests}
    end
  end

  defp process_response({:error, ref, reason}, data) do
    case Map.pop(data.requests, ref) do
      {nil, _} ->
        data

      {req, requests} ->
        :gen_statem.reply(req.from, {:error, reason})
        %{data | requests: requests}
    end
  end

  defp process_response(_other, data), do: data

  defp fail_all(requests, reason) do
    Enum.each(requests, fn {_ref, req} ->
      :gen_statem.reply(req.from, {:error, reason})
    end)
  end

  defp next_backoff(0), do: 1_000
  defp next_backoff(n) when n < 30_000, do: min(n * 2, 30_000)
  defp next_backoff(_), do: 30_000

  defp safe_close(nil), do: :ok

  defp safe_close(conn) do
    try do
      HTTP.close(conn)
    catch
      _, _ -> :ok
    end
  end

  # silence unused warnings
  _ = @connect_timeout
end
