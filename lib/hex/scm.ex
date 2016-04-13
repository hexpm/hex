defmodule Hex.SCM do
  @moduledoc false

  @behaviour Mix.SCM
  @packages_dir "packages"
  @request_timeout 60_000
  @fetch_timeout @request_timeout * 2

  def fetchable? do
    true
  end

  def format(_opts) do
    "Hex package"
  end

  def format_lock(opts) do
    case Hex.Utils.lock(opts[:lock]) do
      [:hex, name, version, nil] ->
        "#{version} (#{name})"
      [:hex, name, version, <<checksum::binary-8, _::binary>>] ->
        "#{version} (#{name}) #{checksum}"
      _ ->
        nil
    end
  end

  def accepts_options(name, opts) do
    Keyword.put_new(opts, :hex, name)
  end

  def checked_out?(opts) do
    File.dir?(opts[:dest])
  end

  def lock_status(opts) do
    case Hex.Utils.lock(opts[:lock]) do
      [:hex, name, version, checksum] ->
        lock_status(opts[:dest], Atom.to_string(name), version, checksum)
      nil ->
        :mismatch
      _ ->
        :outdated
    end
  end

  defp lock_status(dest, name, version, checksum) do
    case File.read(Path.join(dest, ".hex")) do
      {:ok, file} ->
        case parse_manifest(file) do
          {^name, ^version, ^checksum} -> :ok
          {^name, ^version, _} when is_nil(checksum) -> :ok
          {^name, ^version} -> :ok
          _ ->
            :mismatch
        end
      {:error, _} ->
        :mismatch
    end
  end

  def equal?(opts1, opts2) do
    opts1[:hex] == opts2[:hex]
  end

  def managers(opts) do
    Hex.Registry.open!(Hex.Registry.ETS)

    case Hex.Utils.lock(opts[:lock]) do
      [:hex, name, version] ->
        name        = Atom.to_string(name)
        build_tools = Hex.Registry.get_build_tools(name, version) || []
        Enum.map(build_tools, &String.to_atom/1)
      _ ->
        []
    end
  after
    Hex.Registry.pdict_clean
  end

  def checkout(opts) do
    Hex.Registry.open!(Hex.Registry.ETS)

    [:hex, _name, version, checksum] = Hex.Utils.lock(opts[:lock])
    name     = opts[:hex]
    dest     = opts[:dest]
    filename = "#{name}-#{version}.tar"
    path     = cache_path(filename)
    url      = Hex.API.repo_url("tarballs/#{filename}")

    Hex.Shell.info "  Checking package (#{url})"

    case Hex.Parallel.await(:hex_fetcher, {name, version}, @fetch_timeout) do
      {:ok, :cached} ->
        Hex.Shell.info "  Using locally cached package"
      {:ok, :offline} ->
        Hex.Shell.info "  [OFFLINE] Using locally cached package"
      {:ok, :new} ->
        Hex.Shell.info "  Fetched package"
      {:error, reason} ->
        Hex.Shell.error(reason)
        unless File.exists?(path) do
          Mix.raise "Package fetch failed and no cached copy available"
        end
        Hex.Shell.info "  Fetch failed. Using locally cached package"
    end

    File.rm_rf!(dest)
    Hex.Tar.unpack(path, dest, {name, version}, checksum)
    manifest = encode_manifest(name, version, checksum)
    File.write!(Path.join(dest, ".hex"), manifest)

    opts[:lock]
  after
    Hex.Registry.pdict_clean
  end

  def update(opts) do
    checkout(opts)
  end

  defp parse_manifest(file) do
    file
    |> String.strip
    |> String.split(",")
    |> List.to_tuple
  end

  defp encode_manifest(name, version, checksum) do
    "#{name},#{version},#{checksum}"
  end

  defp cache_path do
    Path.join(Hex.State.fetch!(:home), @packages_dir)
  end

  defp cache_path(name) do
    Path.join([Hex.State.fetch!(:home), @packages_dir, name])
  end

  def prefetch(lock) do
    fetch = fetch_from_lock(lock)

    Enum.each(fetch, fn {name, version} ->
      Hex.Parallel.run(:hex_fetcher, {name, version}, fn ->
        filename = "#{name}-#{version}.tar"
        path = cache_path(filename)
        fetch(filename, path)
      end)
    end)
  end

  defp fetch_from_lock(lock) do
    deps_path = Mix.Project.deps_path

    Enum.flat_map(lock, fn {_app, info} ->
      case Hex.Utils.lock(info) do
        [:hex, name, version, _checksum] ->
          if fetch?(name, version, deps_path), do: [{name, version}], else: []
        _ ->
          []
      end
    end)
  end

  defp fetch?(name, version, deps_path) do
    dest = Path.join(deps_path, "#{name}")

    case File.read(Path.join(dest, ".hex")) do
      {:ok, contents} ->
        {name, version} != parse_manifest(contents)
      {:error, _} ->
        true
    end
  end

  defp fetch(name, path) do
    if Hex.State.fetch!(:offline?) do
      {:ok, :offline}
    else
      etag = Hex.Utils.etag(path)
      url  = Hex.API.repo_url("tarballs/#{name}")
      File.mkdir_p!(cache_path)

      case request(url, etag) do
        {:ok, body} when is_binary(body) ->
          File.write!(path, body)
          {:ok, :new}
        other ->
          other
      end
    end
  end

  defp request(url, etag) do
    opts = [body_format: :binary]
    headers = [{'user-agent', Hex.API.user_agent}]
    headers = if etag, do: [{'if-none-match', '"' ++ etag ++ '"'}|headers], else: headers
    http_opts = [ssl: Hex.API.ssl_opts(url), relaxed: true, timeout: @request_timeout] ++ Hex.Utils.proxy_config(url)
    url = String.to_char_list(url)

    case :httpc.request(:get, {url, headers}, http_opts, opts, :hex) do
      {:ok, {{_version, 200, _reason}, _headers, body}} ->
        {:ok, body}
      {:ok, {{_version, 304, _reason}, _headers, _body}} ->
        {:ok, :cached}
      {:ok, {{_version, code, _reason}, _headers, _body}} ->
        {:error, "Request failed (#{code})"}
      {:error, reason} ->
        {:error, "Request failed (#{inspect reason})"}
    end
  end
end
