defmodule Hex.HTTP.Pool do
  @moduledoc false

  # Top-level Mint-based HTTP connection pool.
  #
  # Owns a Registry and a DynamicSupervisor. Each {scheme, host, port} maps to
  # one `Hex.HTTP.Pool.Host` child which owns its own pool of
  # `Hex.HTTP.Pool.Conn` processes. Protocol (HTTP/1 vs HTTP/2) is discovered
  # inside the Conn on its first connect, so this layer is protocol-agnostic.

  use Supervisor

  alias Hex.HTTP.Pool.Host

  @registry Hex.HTTP.Pool.Registry
  @dyn_sup Hex.HTTP.Pool.DynamicSupervisor

  def start_link(_opts \\ []) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init(_) do
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

  The pool key includes the IPv4/IPv6 variant (derived from `connect_opts`'
  `:inet4`/`:inet6` transport flags) so `Hex.HTTP`'s IPv4↔IPv6 retry fallback
  actually takes effect: a retry with the opposite inet variant gets its own
  Host pool and connects with the new flags, rather than being routed to the
  existing pool that still carries the original variant's connect_opts.
  """
  def request(url, method, headers, body, opts) do
    {scheme, host, port, path} = parse_url(url)
    connect_opts = Keyword.get(opts, :connect_opts, [])
    inet = inet_variant(connect_opts)
    key = {scheme, host, port, inet}

    timeout = Keyword.get(opts, :timeout, 15_000)

    pid = get_or_start_host(key, connect_opts)
    Host.request(pid, method, path, headers, body, timeout)
  end

  defp inet_variant(connect_opts) do
    transport_opts = Keyword.get(connect_opts, :transport_opts, [])

    case {Keyword.get(transport_opts, :inet4, true), Keyword.get(transport_opts, :inet6, false)} do
      {true, false} -> :inet
      {false, true} -> :inet6
      _ -> :default
    end
  end

  defp get_or_start_host(key, connect_opts) do
    case Registry.lookup(@registry, key) do
      [{pid, _}] -> pid
      [] -> start_host(key, connect_opts)
    end
  end

  defp start_host(key, connect_opts) do
    via = {:via, Registry, {@registry, key}}
    arg = {key, connect_opts, [name: via]}

    spec = %{
      id: {Host, key},
      start: {Host, :start_link, [arg]},
      restart: :temporary
    }

    case DynamicSupervisor.start_child(@dyn_sup, spec) do
      {:ok, pid} ->
        pid

      {:error, {:already_started, pid}} ->
        pid

      {:error, reason} ->
        case Registry.lookup(@registry, key) do
          [{pid, _}] ->
            pid

          [] ->
            raise "failed to start Hex HTTP host pool for #{inspect(key)}: #{inspect(reason)}"
        end
    end
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
end
