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

  def accepts_options(opts) do
    if opts[:package], do: opts
  end

  def checked_out?(opts) do
    File.dir?(opts[:dest])
  end

  def lock_status(opts) do
    case opts[:lock] do
      { :package, version } ->
        Mix.Deps.in_dependency(%Mix.Dep{app: opts[:app], opts: opts}, fn _ ->
          if Mix.project[:version] == version do
            :ok
          else
            :mismatch
          end
        end)
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
    app  = opts[:app]
    dest = opts[:dest]
    { :package, version } = opts[:lock]
    fetch(app, version)
    Mix.shell.info("Unpacking tarball...")

    File.rm_rf!(dest)
    unpack(app, version, dest)
    Mix.shell.info("Successfully unpacked")
    opts[:lock]
  end

  def update(opts) do
    checkout(opts)
  end

  defp unpack(package, version, dest) do
    name = "#{package}-#{version}.tar"
    path = cache_path(name)

    # TODO: Validate package with checksum, version, etc.
    # note that if tarball is empty :ok is returned, not { :ok, [] }

    case :erl_tar.extract(path, [:memory, files: ['contents.tar.gz']]) do
      { :ok, [{ _name, binary }] } ->
        case :erl_tar.extract({ :binary, binary }, [:compressed, cwd: dest]) do
          :ok ->
            :ok
        { :error, reason } ->
          raise Mix.Error, message: "Unpacking #{path}/contents.tar.gz failed: #{inspect reason}"
        end
      { :error, reason } ->
        raise Mix.Error, message: "Unpacking #{path} failed: #{inspect reason}"
    end
  end

  defp fetch(package, version) do
    name = "#{package}-#{version}.tar"
    path = cache_path(name)
    etag = etag(path)
    url  = Hex.API.cdn("tarballs/#{name}")

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

  defp etag(path) do
    case File.read(path) do
      { :ok, binary } ->
        :crypto.hash(:md5, binary)
        |> Hex.Util.hexify
        |> String.to_char_list!
      { :error, _ } ->
        nil
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

