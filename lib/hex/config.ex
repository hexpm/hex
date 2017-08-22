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

  def read() do
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
    config
  end

  defp config_path() do
    Path.join(hex_home(), "hex.config")
  end

  defp hex_home() do
    state_pid = Process.whereis(Hex.State)
    if state_pid && state_pid != self() do
      Hex.State.fetch!(:home)
    else
      Path.expand(System.get_env("HEX_HOME") || "~/.hex")
    end
  end

  defp encode_term(list) do
    list
    |> Enum.map(&[:io_lib.print(&1) | ".\n"])
    |> IO.iodata_to_binary()
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
    |> merge_hexpm()
    |> update_organizations()
  end

  def update_repos(repos) do
    Hex.Config.update([{:"$repos", repos}])
    Hex.State.put(:"$repos", repos)
  end

  defp default_hexpm() do
    %{
      url: @hexpm_url,
      public_key: @hexpm_public_key,
      auth_key: nil,
      organization: nil,
    }
  end

  defp merge_hexpm(repos) do
    hexpm = default_hexpm()
    Map.update(repos, "hexpm", hexpm, &Map.merge(hexpm, &1))
  end

  defp update_organizations(repos) do
    Enum.into(repos, %{}, fn {key, repo} ->
      if repo.organization do
        source = Map.fetch!(repos, repo.organization)
        repo =
          repo
          |> Map.put(:url, merge_values(repo.url, source.url <> "/repos/#{key}"))
          |> Map.put(:public_key, merge_values(repo.public_key, source.public_key))
        {key, repo}
      else
        {key, repo}
      end
    end)
  end

  defp merge_values(nil, right), do: right
  defp merge_values(left, _right), do: left
end
