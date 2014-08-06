defmodule Hex.SCM do
  @moduledoc false

  @behaviour Mix.SCM
  @packages_dir "packages"

  def fetchable? do
    true
  end

  def format(_opts) do
    "package"
  end

  def format_lock(opts) do
    case opts[:lock] do
      {:package, version} ->
        version
      _ ->
        nil
    end
  end

  def accepts_options(app, opts) do
    Keyword.put(opts, :hex_app, app)
  end

  def checked_out?(opts) do
    File.dir?(opts[:dest])
  end

  def lock_status(opts) do
    case opts[:lock] do
      {:package, version} ->
        case File.read(Path.join(opts[:dest], ".hex")) do
          {:ok, dep_version} ->
            if String.strip(dep_version) == version, do: :ok, else: :mismatch
          {:error, _} ->
            :mismatch
        end
      nil ->
        :mismatch
      _ ->
        :outdated
    end
  end

  def equal?(opts1, opts2) do
    opts1[:package] == opts2[:package]
  end

  def checkout(opts) do
    Hex.Util.move_home

    {:package, version} = opts[:lock]
    app  = opts[:hex_app]
    dest = opts[:dest]
    name = "#{app}-#{version}.tar"
    path = cache_path(name)
    url  = Hex.API.cdn_url("tarballs/#{name}")

    Mix.shell.info("Checking package (#{url})")

    # TODO: Increase this timeout when http timeouts have been fixed
    case Hex.Parallel.await(:hex_fetcher, {app, version}, 5000) do
      {:ok, :cached} ->
        Mix.shell.info("Using locally cached package")
      {:ok, :new} ->
        Mix.shell.info("Fetched package")
      {:error, reason} ->
        Mix.shell.error(reason)
        unless File.exists?(path) do
          Mix.raise "Package fetch failed and no cached copy available"
        end
        Mix.shell.info("Check failed. Using locally cached package")
    end

    File.rm_rf!(dest)
    Hex.Tar.unpack(path, dest)
    File.write!(Path.join(dest, ".hex"), version)

    Mix.shell.info("Unpacked package tarball (#{path})")
    opts[:lock]
  end

  def update(opts) do
    checkout(opts)
  end

  defp cache_path do
    Path.join(Hex.home, @packages_dir)
  end

  defp cache_path(name) do
    Path.join([Hex.home, @packages_dir, name])
  end

  def prefetch(lock) do
    fetch = fetch_from_lock(lock)

    Enum.each(fetch, fn {app, version} ->
      Hex.Parallel.run(:hex_fetcher, {app, version}, fn ->
        name = "#{app}-#{version}.tar"
        path = cache_path(name)
        fetch(name, path)
      end)
    end)
  end

  defp fetch_from_lock(lock) do
    deps_path = Mix.Project.deps_path

    Enum.flat_map(lock, fn
      {app, {:package, version}} ->
        if fetch?(app, version, deps_path) do
          [{app, version}]
        else
          []
        end
      _ ->
        []
    end)
  end

  defp fetch?(app, version, deps_path) do
    dest = Path.join(deps_path, "#{app}")

    case File.read(Path.join(dest, ".hex")) do
      {:ok, dep_version} ->
        String.strip(dep_version) != version
      {:error, _} ->
        true
    end
  end

  defp fetch(name, path) do
    etag = Hex.Util.etag(path)
    url  = Hex.API.cdn_url("tarballs/#{name}")

    File.mkdir_p!(cache_path)
    File.open!(path, [:write], &request(url, etag, &1))
  end

  defp request(url, etag, file) do
    opts = [sync: false, stream: {:self, :once}]
    headers = [{'user-agent', Hex.API.user_agent}]
    if etag do
      headers = headers ++ [{'if-none-match', etag}]
    end

    case :httpc.request(:get, {url, headers}, [], opts, :hex) do
      {:ok, request_id} when is_reference(request_id) ->
        stream_start(request_id, file)
      {:ok, {{_version, 304, _reason}, _headers, _body}} ->
        {:ok, :cached}
      {:ok, {{_version, code, _reason}, _headers, _body}} ->
        {:error, "Request failed (#{code})"}
      {:error, reason} ->
        {:error, "Request failed: #{inspect reason}"}
    end
  end

  # Minimum 100kb over 10s
  @timeout 10
  @timeout_chunk 100_000

  defp stream_start(request_id, file) do
    receive do
      {:http, {^request_id, :stream_start, _headers, pid}} ->
        :httpc.stream_next(pid)
        state = %{request: request_id, file: file, pid: pid, timeout: {now(), 0}}
        stream(state)
    after
      @timeout * 1000 ->
        {:error, :timeout}
    end
  end

  defp stream(%{request: request, file: file, pid: pid, timeout: {time, size}} = s) do
    case check_timeout(s) do
      {:ok, s} ->
        receive do
          {:http, {^request, :stream, body}} ->
            size = byte_size(body) + size
            IO.binwrite(file, body)
            :httpc.stream_next(pid)
            stream(%{s | timeout: {time, size}})

          {:http, {^request, :stream_end, _headers}} ->
            {:ok, :new}

        after
          1000 ->
            stream(s)
        end

      {:error, _} = err ->
        err
    end
  end

  defp check_timeout(%{timeout: {time, size}} = s) do
    cond do
      size > @timeout_chunk ->
        {:ok, %{s | timeout: {now(), 0}}}
      now() - time > @timeout ->
        {:error, :timeout}
      true ->
        {:ok, s}
    end
  end

  defp now do
    :calendar.local_time
    |> :calendar.datetime_to_gregorian_seconds
  end
end

