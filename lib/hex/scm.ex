defmodule Hex.SCM do
  @moduledoc false

  @behaviour Mix.SCM

  @cache_dir ".package-cache"

  def fetchable? do
    true
  end

  def format(_opts) do
    "package"
  end

  def format_lock(opts) do
    case opts[:lock] do
      { :package, version } ->
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
      { :package, version } ->
        case File.read(Path.join(opts[:dest], ".hex")) do
          { :ok, dep_version } ->
            if String.strip(dep_version) == version, do: :ok, else: :mismatch
          { :error, _ } ->
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
    app  = opts[:hex_app]
    dest = opts[:dest]
    { :package, version } = opts[:lock]
    name = "#{app}-#{version}.tar"
    path = cache_path(name)

    fetch(name, path)
    Mix.shell.info("Unpacking tarball...")

    File.rm_rf!(dest)
    Hex.Tar.unpack(path, dest)
    File.write!(Path.join(dest, ".hex"), version)

    Mix.shell.info("Successfully unpacked")
    opts[:lock]
  end

  def update(opts) do
    checkout(opts)
  end

  defp fetch(name, path) do
    etag = Hex.Util.etag(path)
    url  = Hex.API.cdn_url("tarballs/#{name}")

    Mix.shell.info("Fetching '#{url}'...")

    cond do
      body = fetch_request(url, etag) ->
        File.mkdir_p!(cache_path)
        File.write!(path, body)
      File.exists?(path) ->
        Mix.shell.info("Using local cached copy")
      true ->
        raise Mix.Error, message: "Package fetch failed"
    end
  end

  defp fetch_request(url, etag) do
    headers = [{ 'user-agent', Hex.API.user_agent }]
    if etag do
      headers = headers ++ [{ 'if-none-match', etag }]
    end

    :inets.start
    case :httpc.request(:get, { url, headers }, [], body_format: :binary) do
      { :ok, response } ->
        handle_response(response)
      { :error, reason } ->
        Mix.shell.info("Request failed: #{inspect reason}")
        nil
    end
  end

  defp handle_response({ { _version, 304, _reason }, _headers, _body }) do
    nil
  end

  defp handle_response({ { _version, 200, _reason }, _headers, body }) do
    Mix.shell.info("Successfully fetched")
    body
  end

  defp handle_response({ { _version, code, _reason }, _headers, _body }) do
    Mix.shell.info("Request failed (#{code})")
    nil
  end

  defp cache_path do
    Path.join(Mix.Utils.mix_home, @cache_dir)
  end

  defp cache_path(name) do
    Path.join([Mix.Utils.mix_home, @cache_dir, name])
  end
end

