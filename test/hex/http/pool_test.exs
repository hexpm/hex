defmodule Hex.HTTP.PoolTest do
  use HexTest.Case, async: false

  setup do
    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  defp wait_for_host(key, timeout \\ 500) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait_for_host(key, deadline)
  end

  defp do_wait_for_host(key, deadline) do
    case Registry.lookup(Hex.HTTP.Pool.__registry__(), key) do
      [{pid, _}] ->
        pid

      [] ->
        if System.monotonic_time(:millisecond) >= deadline do
          flunk("no Host registered for #{inspect(key)}")
        else
          Process.sleep(10)
          do_wait_for_host(key, deadline)
        end
    end
  end

  defp wait_until(fun, timeout \\ 1_000) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait_until(fun, deadline)
  end

  defp do_wait_until(fun, deadline) do
    case fun.() do
      {:ok, result} ->
        result

      :retry ->
        if System.monotonic_time(:millisecond) >= deadline do
          flunk("condition not met before timeout")
        else
          Process.sleep(10)
          do_wait_until(fun, deadline)
        end
    end
  end

  test "different inet variants get separate Host pools", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "ok") end)

    url = "http://localhost:#{bypass.port}/"

    pool_opts_inet4 = [
      timeout: 5_000,
      connect_opts: [transport_opts: [inet4: true, inet6: false]]
    ]

    pool_opts_inet6 = [
      timeout: 5_000,
      connect_opts: [transport_opts: [inet4: false, inet6: true]]
    ]

    {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts_inet4)

    pid_inet4 = wait_for_host({:http, "localhost", bypass.port, :inet})

    # IPv6 connect likely fails since bypass binds to IPv4; the request will
    # error but a separate Host registration must still have been attempted.
    _ = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts_inet6)

    pid_inet6 = wait_for_host({:http, "localhost", bypass.port, :inet6})

    assert pid_inet4 != pid_inet6
  end

  test "Host spawns a replacement Conn after one crashes", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "ok") end)

    url = "http://localhost:#{bypass.port}/"
    pool_opts = [timeout: 5_000]

    {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)

    host_pid = wait_for_host({:http, "localhost", bypass.port, :inet})

    # Wait for both probe Conns to report ready.
    wait_until(fn ->
      state = :sys.get_state(host_pid)
      ready_count = Enum.count(state.conns, fn {_, info} -> info.ready end)
      if ready_count >= 2, do: {:ok, state}, else: :retry
    end)

    state_before = :sys.get_state(host_pid)
    conn_pids_before = Map.keys(state_before.conns)
    [victim | _] = conn_pids_before

    Process.exit(victim, :kill)

    # Host traps exits and must spawn a replacement — pool size stays stable
    # and the victim pid is no longer a member.
    wait_until(fn ->
      state = :sys.get_state(host_pid)
      members = Map.keys(state.conns)

      cond do
        victim in members ->
          :retry

        map_size(state.conns) != map_size(state_before.conns) ->
          :retry

        true ->
          {:ok, state}
      end
    end)

    # The pool remains functional after the crash.
    {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)
  end

  test "requests queue when all Conns are busy and drain as they finish", %{bypass: bypass} do
    # Slow every response so we can saturate a small pool.
    test_pid = self()

    Bypass.expect(bypass, fn conn ->
      send(test_pid, {:bypass_got, conn.request_path})
      Process.sleep(100)
      Plug.Conn.resp(conn, 200, "ok")
    end)

    url_base = "http://localhost:#{bypass.port}"
    pool_opts = [timeout: 5_000]

    # Warm the pool to HTTP/1 (bypass is HTTP/1 only), then spawn more
    # requests than pool capacity (default 8) so the 9th+ must queue.
    {:ok, 200, _, _} = Hex.HTTP.Pool.request(url_base <> "/warmup", "GET", [], nil, pool_opts)

    count = 12

    tasks =
      for i <- 1..count do
        Task.async(fn ->
          Hex.HTTP.Pool.request(url_base <> "/req#{i}", "GET", [], nil, pool_opts)
        end)
      end

    results = Task.await_many(tasks, 10_000)

    assert length(results) == count
    Enum.each(results, fn {:ok, 200, _, "ok"} -> :ok end)
  end
end
