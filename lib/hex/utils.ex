defmodule Hex.Utils do
  def ensure_registry(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.path) do
      {:error, :update_failed}
    else
      start_result = Hex.Registry.open

      # Show available newer versions
      if update_result in [{:ok, :new}, {:ok, :no_fetch}] and start_result == :ok do
        Hex.Registry.info_installs
      end

      start_result
    end
  end

  def ensure_registry!(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.path) do
      Mix.raise "Failed to fetch registry"
    end

    Hex.Registry.open!

    # Show available newer versions
    if update_result in [{:ok, :new}, {:ok, :no_fetch}] do
      Hex.Registry.info_installs
    end
  end

  defp update_registry(opts) do
    cond do
      Hex.State.fetch!(:offline?) ->
        {:ok, :offline}
      Hex.State.fetch!(:registry_updated) ->
        {:ok, :cached}
      true ->
        Hex.State.put(:registry_updated, true)

        closed? = Hex.Registry.close
        path    = Hex.Registry.path
        path_gz = path <> ".gz"
        fetch?  = Keyword.get(opts, :fetch, true) and
                  (Keyword.get(opts, :update, true) or not week_fresh?(path_gz))
        try do
          if fetch? do
            api_opts =
              if Keyword.get(opts, :cache, true) do
                [etag: etag(path_gz)]
              else
                []
              end

            case Hex.API.Registry.get(api_opts) do
              {200, body} ->
                File.mkdir_p!(Path.dirname(path))
                File.write!(path_gz, body)
                data = :zlib.gunzip(body)
                File.write!(path, data)
                {:ok, :new}
              {304, _} ->
                {:ok, :new}
              {code, body} ->
                Hex.Shell.error "Registry update failed (#{code})"
                print_error_result(code, body)
                :error
            end
          else
            {:ok, :no_fetch}
          end
        after
          # Open registry if it was already open when update began
          if closed?, do: Hex.Registry.open!
        end
    end
  end

  @week_seconds 7 * 24 * 60 * 60

  def week_fresh?(path) do
    case File.stat(path) do
      {:ok, %File.Stat{mtime: mtime}} ->
        now   = :calendar.local_time |> :calendar.datetime_to_gregorian_seconds
        mtime = mtime                |> :calendar.datetime_to_gregorian_seconds

        now - mtime < @week_seconds
      {:error, _} ->
        false
    end
  end

  def etag(path) do
    case File.read(path) do
      {:ok, binary} ->
        :crypto.hash(:md5, binary)
        |> Base.encode16(case: :lower)
        |> String.to_char_list
      {:error, _} ->
        nil
    end
  end

  def safe_deserialize_erlang("") do
    nil
  end

  def safe_deserialize_erlang(binary) do
    try do
      :erlang.binary_to_term(binary, [:safe])
    rescue
      ArgumentError ->
        Mix.raise "Received malformed erlang from Hex API"
    end
  end

  def safe_serialize_erlang(term) do
    binarify(term)
    |> :erlang.term_to_binary
  end

  def binarify(term, opts \\ [])

  def binarify(binary, _opts) when is_binary(binary),
    do: binary
  def binarify(number, _opts) when is_number(number),
    do: number
  def binarify(atom, _opts) when is_nil(atom) or is_boolean(atom),
    do: atom
  def binarify(atom, _opts) when is_atom(atom),
    do: Atom.to_string(atom)
  def binarify(list, opts) when is_list(list),
    do: for(elem <- list, do: binarify(elem, opts))
  def binarify(tuple, opts) when is_tuple(tuple),
    do: for(elem <- Tuple.to_list(tuple), do: binarify(elem, opts)) |> List.to_tuple
  def binarify(map, opts) when is_map(map) do
    if Keyword.get(opts, :maps, true) do
      for(elem <- map, into: %{}, do: binarify(elem, opts))
    else
      for(elem <- map, do: binarify(elem, opts))
    end
  end

  def print_error_result(:http_error, reason),
    do: Hex.Shell.info inspect(reason)
  def print_error_result(status, nil),
    do: print_http_code(status)
  def print_error_result(status, ""),
    do: print_http_code(status)

  def print_error_result(_status, body) when is_binary(body) do
    Hex.Shell.info body
  end

  def print_error_result(status, body) do
    message = body["message"]
    errors = body["errors"]

    if message do
      Hex.Shell.info message
    end

    if errors do
      pretty_errors(errors)
    end

    unless message || errors do
      print_http_code(status)
      Hex.Shell.info body
    end
  end

  defp pretty_errors(errors, depth \\ 0) do
    Enum.each(errors, fn
      {key, map} when is_map(map) ->
        Hex.Shell.info indent(depth) <> key <> ":"
        pretty_errors(map, depth + 1)
      {key, value} ->
        Hex.Shell.info indent(depth) <> key <> ": " <> value
    end)
  end

  defp print_http_code(code), do: Hex.Shell.info pretty_http_code(code)

  defp pretty_http_code(401), do: "Authentication failed (401)"
  defp pretty_http_code(403), do: "Forbidden (403)"
  defp pretty_http_code(404), do: "Entity not found (404)"
  defp pretty_http_code(422), do: "Validation failed (422)"
  defp pretty_http_code(code), do: "HTTP status code: #{code}"

  defp indent(0), do: "  "
  defp indent(depth), do: "  " <> indent(depth - 1)

  def hex_package_url(package),
    do: "https://hex.pm/packages/#{package}"
  def hex_package_url(package, version),
    do: "https://hex.pm/packages/#{package}/#{version}"
    
  def hexdocs_url(package),
    do: "http://hexdocs.pm/#{package}"
  def hexdocs_url(package, version),
    do: "http://hexdocs.pm/#{package}/#{version}"

  def proxy_config(url) do
    {http_proxy, https_proxy} = proxy_setup
    proxy_auth(URI.parse(url), http_proxy, https_proxy)
  end

  defp proxy_setup do
    http_proxy  = (proxy = Hex.State.get(:http_proxy))  && proxy(:http, proxy)
    https_proxy = (proxy = Hex.State.get(:https_proxy)) && proxy(:https, proxy)
    {http_proxy, https_proxy}
  end

  defp proxy(scheme, proxy) do
    uri = URI.parse(proxy)

    if uri.host && uri.port do
      host = String.to_char_list(uri.host)
      :httpc.set_options([{proxy_scheme(scheme), {{host, uri.port}, []}}], :hex)
    end

    uri
  end

  defp proxy_scheme(scheme) do
    case scheme do
      :http  -> :proxy
      :https -> :https_proxy
    end
  end

  defp proxy_auth(%URI{scheme: "http"}, http_proxy, _https_proxy),
    do: proxy_auth(http_proxy)
  defp proxy_auth(%URI{scheme: "https"}, _http_proxy, https_proxy),
    do: proxy_auth(https_proxy)

  defp proxy_auth(nil),
    do: []
  defp proxy_auth(%URI{userinfo: nil}),
    do: []
  defp proxy_auth(%URI{userinfo: auth}) do
    destructure [user, pass], String.split(auth, ":", parts: 2)

    user = String.to_char_list(user)
    pass = String.to_char_list(pass || "")

    [proxy_auth: {user, pass}]
  end

  # From https://github.com/fishcakez/dialyze/blob/6698ae582c77940ee10b4babe4adeff22f1b7779/lib/mix/tasks/dialyze.ex#L168
  def otp_version do
    major = :erlang.system_info(:otp_release) |> List.to_string
    vsn_file = Path.join([:code.root_dir(), "releases", major, "OTP_VERSION"])
    try do
      {:ok, contents} = File.read(vsn_file)
      String.split(contents, "\n", trim: true)
    else
      [full] ->
        full
      _ ->
        major
    catch
      :error, _ ->
        major
    end
  end
end
