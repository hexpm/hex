defmodule Hex.Utils do
  @public_keys_html "https://hex.pm/docs/public_keys"

  def ensure_registry(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.ETS.path) do
      {:error, :update_failed}
    else
      start_result =
        if Keyword.get(opts, :open, true),
          do: Hex.Registry.open(Hex.Registry.ETS),
        else: :nope

      # Show available newer versions
      if update_result in [{:ok, :new}, {:ok, :no_fetch}] and start_result == :ok do
        Hex.Registry.info_installs
      end

      start_result
    end
  end

  def ensure_registry!(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.ETS.path) do
      Mix.raise "Failed to fetch registry"
    end

    start_result =
      if Keyword.get(opts, :open, true),
        do: Hex.Registry.open!(Hex.Registry.ETS),
      else: :nope

    # Show available newer versions
    if update_result in [{:ok, :new}, {:ok, :no_fetch}] and start_result == :ok do
      Hex.Registry.info_installs
    end
  end

  defp update_registry(opts) do
    path    = Hex.Registry.ETS.path
    path_gz = path <> ".gz"

    cond do
      Hex.State.fetch!(:offline?) ->
        {:ok, :offline}
      Hex.State.fetch!(:registry_updated) ->
        {:ok, :cached}
      not Keyword.get(opts, :fetch, true) ->
        {:ok, :no_fetch}
      true ->
        Hex.State.put(:registry_updated, true)
        closed? = Hex.Registry.close

        try do
          api_opts =
            if Keyword.get(opts, :cache, true) do
              [etag: etag(path_gz)]
            else
              []
            end

          case Hex.API.Registry.get(api_opts) do
            {200, body, headers} ->
              Hex.State.fetch!(:check_registry?) && verify_registry!(body, headers)
              File.mkdir_p!(Path.dirname(path))
              File.write!(path_gz, body)
              data = :zlib.gunzip(body)
              File.write!(path, data)
              {:ok, :new}
            {304, _, _} ->
              {:ok, :new}
            {code, body, _} ->
              Hex.Shell.error "Registry update failed (#{code})"
              print_error_result(code, body)
              :error
          end
        after
          # Open registry if it was already open when update began
          if closed?, do: Hex.Registry.open!(Hex.Registry.ETS)
        end
    end
  end

  defp verify_registry!(body, headers) do
    domain = if repo = Hex.State.fetch!(:repo), do: repo, else: "hex.pm"

    signature = headers['x-hex-signature'] ||
                headers['x-amz-meta-signature'] ||
                get_signature(domain)

    signature = signature |> to_string |> Base.decode16!(case: :lower)
    key = Hex.PublicKey.public_keys(domain)

    unless key do
      Mix.raise "No public key stored for #{domain}. Either install a public " <>
                "key with `mix hex.public_keys` or disable the registry " <>
                "verification check by setting `HEX_UNSAFE_REGISTRY=1`."
    end

    unless Hex.PublicKey.verify(body, :sha512, signature, [key]) do
      Mix.raise "Could not verify authenticity of fetched registry file. " <>
                "This may happen because a proxy or some entity is " <>
                "interfering with the download or because you don't have a " <>
                "public key to verify the registry.\n\nYou may try again " <>
                "later or check if a new public key has been released in " <>
                "our public keys page: #{@public_keys_html}"
    end
  end

  defp get_signature(domain) do
    case Hex.API.Registry.get_signature do
      {200, body, _} ->
        body
      other ->
        # TODO: We should be able to disable the registry verification per domain.
        #       No reason to make all registries unsafe.
        reason = signature_fetch_fail(other)
        Mix.raise "The repository at #{domain} did not provide a signature " <>
                  "for the registry because it #{reason}. This could be because " <>
                  "of a man-in-the-middle attack or simply because the repository " <>
                  "does not sign its registry. The signature verification check " <>
                  "can be disabled by setting `HEX_UNSAFE_REGISTRY=1`."
    end
  end

  defp signature_fetch_fail({:http_error, reason, _}),
    do: "failed with http error #{inspect reason}"
  defp signature_fetch_fail({code, _, _}),
    do: "returned http status code #{code}"

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
    do: "https://hexdocs.pm/#{package}"
  def hexdocs_url(package, version),
    do: "https://hexdocs.pm/#{package}/#{version}"

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

  def lock(nil), do: nil
  def lock({:hex, name, version}), do: [:hex, name, version, nil]
  def lock(tuple), do: tuple |> Tuple.to_list |> Enum.take(4)
end
