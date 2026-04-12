defmodule Hex.HTTP.Pool do
  @moduledoc false

  # Top-level Mint-based HTTP connection pool.
  #
  # Responsibilities:
  #   * Supervises a Registry and a DynamicSupervisor for per-host pools
  #   * Owns an ETS table caching the protocol (:http1 | :http2) discovered
  #     via ALPN for each {scheme, host, port} tuple
  #   * Routes `request/5` to the appropriate HTTP/1 or HTTP/2 pool,
  #     probing the server the first time we see a host

  use Supervisor

  alias Hex.HTTP.Pool.{HTTP1, HTTP2}
  alias Hex.Mint.HTTP, as: MintHTTP

  @registry Hex.HTTP.Pool.Registry
  @dyn_sup Hex.HTTP.Pool.DynamicSupervisor
  @ets :hex_http_pool_protocol

  def start_link(_opts \\ []) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init(_) do
    _ =
      try do
        :ets.new(@ets, [:named_table, :public, :set, read_concurrency: true])
      rescue
        ArgumentError -> :ok
      end

    children = [
      {Registry, keys: :unique, name: @registry},
      {DynamicSupervisor, name: @dyn_sup, strategy: :one_for_one}
    ]

    Supervisor.init(children, strategy: :rest_for_one)
  end

  @doc """
  Performs an HTTP request via the appropriate per-host pool.

  Args:
    * `url` - request URL (binary)
    * `method` - binary method (e.g. "GET")
    * `headers` - list of `{name, value}` tuples
    * `body` - binary body or nil
    * `opts` - keyword list, supports:
        * `:timeout` - total timeout ms (default 15_000)
        * `:connect_opts` - keyword list passed to `Hex.Mint.HTTP.connect/4`
  """
  def request(url, method, headers, body, opts) do
    {scheme, host, port, path} = parse_url(url)

    do_request({scheme, host, port}, path, method, headers, body, opts)
  end

  defp do_request(key, path, method, headers, body, opts) do
    case :ets.lookup(@ets, key) do
      [{^key, :http1}] ->
        request_http1(key, path, method, headers, body, opts, nil)

      [{^key, :http2}] ->
        case request_http2(key, path, method, headers, body, opts, nil) do
          {:error, :read_only} ->
            _ = stop_pool(key)
            do_request(key, path, method, headers, body, opts)

          other ->
            other
        end

      [] ->
        probe_and_dispatch(key, path, method, headers, body, opts)
    end
  end

  defp probe_and_dispatch(key, path, method, headers, body, opts) do
    connect_opts = Keyword.get(opts, :connect_opts, [])

    case probe_connect(key, connect_opts) do
      {:ok, conn, :http1} ->
        :ets.insert(@ets, {key, :http1})
        request_http1(key, path, method, headers, body, opts, conn)

      {:ok, conn, :http2} ->
        :ets.insert(@ets, {key, :http2})
        request_http2(key, path, method, headers, body, opts, conn)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp probe_connect({scheme, host, port}, connect_opts) do
    opts = Keyword.merge([protocols: [:http1, :http2]], connect_opts)

    case MintHTTP.connect(scheme, host, port, opts) do
      {:ok, conn} ->
        {:ok, conn, MintHTTP.protocol(conn)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp request_http1({scheme, host, port} = key, path, method, headers, body, opts, initial_conn) do
    connect_opts = Keyword.get(opts, :connect_opts, [])
    timeout = Keyword.get(opts, :timeout, 15_000)

    connect_fun = fn ->
      MintHTTP.connect(
        scheme,
        host,
        port,
        Keyword.merge([protocols: [:http1]], connect_opts)
      )
    end

    pid = get_or_start_pool(key, :http1, connect_fun, [])

    HTTP1.request(pid, method, path, headers, body,
      timeout: timeout,
      initial_conn: initial_conn
    )
  end

  defp request_http2({scheme, host, port} = key, path, method, headers, body, opts, initial_conn) do
    connect_opts = Keyword.get(opts, :connect_opts, [])
    timeout = Keyword.get(opts, :timeout, 15_000)

    connect_fun = fn ->
      MintHTTP.connect(
        scheme,
        host,
        port,
        Keyword.merge([protocols: [:http2]], connect_opts)
      )
    end

    pid =
      get_or_start_pool(key, :http2, connect_fun, initial_conn: initial_conn)

    HTTP2.request(pid, method, path, headers, body, timeout)
  end

  defp get_or_start_pool(key, protocol, connect_fun, extra_opts) do
    reg_key = {protocol, key}

    case Registry.lookup(@registry, reg_key) do
      [{pid, _}] ->
        pid

      [] ->
        start_pool(reg_key, protocol, key, connect_fun, extra_opts)
    end
  end

  defp start_pool(reg_key, protocol, key, connect_fun, extra_opts) do
    module =
      case protocol do
        :http1 -> HTTP1
        :http2 -> HTTP2
      end

    via = {:via, Registry, {@registry, reg_key}}
    arg = {key, connect_fun, Keyword.put(extra_opts, :name, via)}

    spec = %{
      id: {module, reg_key},
      start: {module, :start_link, [arg]},
      restart: :temporary
    }

    case DynamicSupervisor.start_child(@dyn_sup, spec) do
      {:ok, pid} ->
        pid

      {:error, {:already_started, pid}} ->
        pid

      {:error, reason} ->
        case Registry.lookup(@registry, reg_key) do
          [{pid, _}] -> pid
          [] -> raise "failed to start HTTP pool for #{inspect(reg_key)}: #{inspect(reason)}"
        end
    end
  end

  defp stop_pool({_, _, _} = key) do
    for protocol <- [:http1, :http2] do
      case Registry.lookup(@registry, {protocol, key}) do
        [{pid, _}] -> DynamicSupervisor.terminate_child(@dyn_sup, pid)
        [] -> :ok
      end
    end

    :ets.delete(@ets, key)
  end

  ## URL parsing

  defp parse_url(url) when is_binary(url) do
    uri = URI.parse(url)
    scheme = String.to_atom(uri.scheme)
    host = uri.host
    port = uri.port || default_port(scheme)

    path =
      (uri.path || "/") <>
        case uri.query do
          nil -> ""
          q -> "?" <> q
        end

    {scheme, host, port, path}
  end

  defp default_port(:http), do: 80
  defp default_port(:https), do: 443

  # For tests / debugging
  @doc false
  def __registry__, do: @registry
  @doc false
  def __ets__, do: @ets
end
