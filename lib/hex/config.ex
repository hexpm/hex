defmodule Hex.Config do
  @hexpm_url "https://repo.hex.pm"
  @hexpm_public_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
  Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
  IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
  3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
  XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
  J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
  0wIDAQAB
  -----END PUBLIC KEY-----
  """

  def read do
    case File.read(config_path()) do
      {:ok, binary} ->
        case decode_term(binary) do
          {:ok, term} ->
            term
          {:error, _} ->
            config = decode_elixir(binary)
            write(config)
            config
        end
      {:error, _} ->
        []
    end
  end

  def update(config) do
    read()
    |> Keyword.merge(config)
    |> write()
  end

  def remove(keys) do
    read()
    |> Keyword.drop(keys)
    |> write()
  end

  def write(config) do
    string = encode_term(config)

    path = config_path()
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, string)
  end

  defp config_path do
    Path.join(hex_home(), "hex.config")
  end

  defp hex_home do
    if Process.whereis(Hex.State) do
      Hex.State.fetch!(:home)
    else
      Path.expand(System.get_env("HEX_HOME") || "~/.hex")
    end
  end

  defp encode_term(list) do
    list
    |> Enum.map(&[:io_lib.print(&1) | ".\n"])
    |> IO.iodata_to_binary
  end

  defp decode_term(string) do
    {:ok, pid} = StringIO.open(string)
    try do
      consult(pid, [], string)
    after
      StringIO.close(pid)
    end
  end

  defp consult(pid, acc, string) when is_pid(pid) do
    case :io.read(pid, '') do
      {:ok, term}      -> consult(pid, [term|acc], string)
      {:error, reason} -> {:error, reason}
      :eof             -> {:ok, Enum.reverse(acc)}
    end
  end

  defp decode_elixir(string) do
    {term, _binding} = Code.eval_string(string)
    term
  end

  def read_repos(config) do
    (config[:"$repos"] || %{})
    |> merge_hexpm
  end

  def update_repos(repos) do
    repos = remove_hexpm(repos)
    Hex.Config.update([{:"$repos", repos}])
  end

  defp default_hexpm do
    %{url: @hexpm_url, public_key: @hexpm_public_key, auth_key: nil}
  end

  defp merge_hexpm(repos) do
    Map.update(repos, "hexpm", default_hexpm(), &Map.merge(default_hexpm(), &1))
  end

  defp remove_hexpm(repos) do
    if Map.get(repos, "hexpm") == default_hexpm() do
      Map.delete(repos, "hexpm")
    else
      repos
    end
  end
end
