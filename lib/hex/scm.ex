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

    case Hex.Parallel.await(:hex_fetcher, {app, version}) do
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
    Hex.Tar.unpack(path, dest, {app, version})
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

    case request(url, etag) do
      {:ok, body} when is_binary(body) ->
        File.write!(path, body)
        {:ok, :new}
      other ->
        other
    end
  end

  defp request(url, etag) do
    opts = [body_format: :binary]
    headers = [{'user-agent', Hex.API.user_agent}]
    if etag do
      headers = headers ++ [{'if-none-match', etag}]
    end

    case :httpc.request(:get, {url, headers}, [], opts, :hex) do
      {:ok, {{_version, 200, _reason}, _headers, body}} ->
        {:ok, body}
      {:ok, {{_version, 304, _reason}, _headers, _body}} ->
        {:ok, :cached}
      {:ok, {{_version, code, _reason}, _headers, _body}} ->
        {:error, "Request failed (#{code})"}
      {:error, reason} ->
        {:error, "Request failed: #{inspect reason}"}
    end
  end
end
