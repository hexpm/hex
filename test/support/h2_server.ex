defmodule HexTest.H2Server do
  # A scripted HTTP/2 server over cleartext TCP with prior knowledge, for
  # testing how the HTTP pool handles server behavior that Bypass can't
  # produce, such as SETTINGS changes and GOAWAY.
  #
  # Every accepted connection is a process that reports to the test process:
  #
  #   * `{:h2_connected, conn}`
  #   * `{:h2_settings, conn, params}` for each SETTINGS frame from the client
  #   * `{:h2_request, conn, stream_id, headers}` for each request
  #   * `{:h2_data, conn, stream_id, data}` for each DATA frame
  #   * `{:h2_closed, conn}` when the client closes the connection
  #
  # The test answers with `respond/4` and `send_frames/2`.

  import Hex.Mint.HTTP2.Frame

  alias Hex.Mint.HPAX
  alias Hex.Mint.HTTP2.Frame

  @preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

  # Options:
  #
  #   * `:settings` - the server's SETTINGS parameters
  #   * `:after_settings` - frames sent together with the server's SETTINGS
  #   * `:accept` - how many connections to accept, later connections stay in
  #     the listen backlog and never get a SETTINGS frame
  def start(opts \\ []) do
    test = self()
    settings = Keyword.get(opts, :settings, [])
    frames = [settings(params: settings) | Keyword.get(opts, :after_settings, [])]
    accept = Keyword.get(opts, :accept, :infinity)

    {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)

    acceptor = spawn(fn -> accept_loop(listen, test, frames, accept) end)
    :ok = :gen_tcp.controlling_process(listen, acceptor)
    ExUnit.Callbacks.on_exit(fn -> Process.exit(acceptor, :kill) end)

    port
  end

  def respond(conn, stream_id, status, body \\ "") do
    send(conn, {:respond, stream_id, status, body})
  end

  def send_frames(conn, frames) do
    send(conn, {:send_frames, frames})
  end

  defp accept_loop(_listen, _test, _frames, 0) do
    Process.sleep(:infinity)
  end

  defp accept_loop(listen, test, frames, accept) do
    {:ok, socket} = :gen_tcp.accept(listen)
    conn = spawn_link(fn -> init_conn(test, frames) end)
    :ok = :gen_tcp.controlling_process(socket, conn)
    send(conn, {:socket, socket})
    accept_loop(listen, test, frames, if(accept == :infinity, do: accept, else: accept - 1))
  end

  defp init_conn(test, frames) do
    socket =
      receive do
        {:socket, socket} -> socket
      end

    :ok = :inet.setopts(socket, active: true)
    :ok = :gen_tcp.send(socket, Enum.map(frames, &Frame.encode/1))
    send(test, {:h2_connected, self()})

    state = %{
      socket: socket,
      test: test,
      buffer: "",
      preface: false,
      decode_table: HPAX.new(4096),
      encode_table: HPAX.new(4096)
    }

    loop(state)
  end

  defp loop(state) do
    receive do
      {:tcp, _socket, data} ->
        state = %{state | buffer: state.buffer <> data}
        loop(handle_data(state))

      {:tcp_closed, _socket} ->
        send(state.test, {:h2_closed, self()})

      {:send_frames, frames} ->
        :ok = :gen_tcp.send(state.socket, Enum.map(frames, &Frame.encode/1))
        loop(state)

      {:respond, stream_id, status, body} ->
        response = [{":status", Integer.to_string(status)}]
        {hbf, encode_table} = HPAX.encode(:store, response, state.encode_table)

        frames =
          if body == "" do
            [headers_frame(stream_id, hbf, [:end_headers, :end_stream])]
          else
            [
              headers_frame(stream_id, hbf, [:end_headers]),
              data(stream_id: stream_id, flags: set_flags(:data, [:end_stream]), data: body)
            ]
          end

        :ok = :gen_tcp.send(state.socket, Enum.map(frames, &Frame.encode/1))
        loop(%{state | encode_table: encode_table})
    end
  end

  defp headers_frame(stream_id, hbf, flags) do
    headers(stream_id: stream_id, flags: set_flags(:headers, flags), hbf: hbf)
  end

  defp handle_data(%{preface: false, buffer: @preface <> rest} = state) do
    handle_data(%{state | preface: true, buffer: rest})
  end

  defp handle_data(%{preface: false} = state), do: state

  defp handle_data(state) do
    case Frame.decode_next(state.buffer) do
      {:ok, frame, rest} -> handle_data(handle_frame(frame, %{state | buffer: rest}))
      :more -> state
    end
  end

  defp handle_frame(settings(flags: flags, params: params), state) do
    unless flag_set?(flags, :settings, :ack) do
      :ok = :gen_tcp.send(state.socket, Frame.encode(settings(flags: 0x01, params: [])))
      send(state.test, {:h2_settings, self(), params})
    end

    state
  end

  defp handle_frame(headers(stream_id: stream_id, hbf: hbf), state) do
    {:ok, headers, decode_table} = HPAX.decode(hbf, state.decode_table)
    send(state.test, {:h2_request, self(), stream_id, headers})
    %{state | decode_table: decode_table}
  end

  defp handle_frame(data(stream_id: stream_id, data: data), state) do
    send(state.test, {:h2_data, self(), stream_id, data})
    state
  end

  defp handle_frame(_frame, state), do: state
end
