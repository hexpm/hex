defmodule Hex.Config do
  @moduledoc false

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
    config = Enum.reject(config, fn {_key, value} -> is_nil(value) end)
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
      Hex.State.fetch!(:config_home)
    else
      Path.expand(System.get_env("HEX_HOME") || "~/.hex")
    end
  end

  def config_home_lookup() do
    case {System.get_env("HEX_HOME"), System.get_env("XDG_CONFIG_HOME")} do
        {directory, _} when is_binary(directory) -> directory
        {nil, directory} when is_binary(directory) -> :filename.basedir(:user_data, "mix")
        {nil, nil} -> Path.expand("~/.mix")
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
      {:ok, term} -> consult(pid, [term | acc], string)
      {:error, reason} -> {:error, reason}
      :eof -> {:ok, Enum.reverse(acc)}
    end
  end

  defp decode_elixir(string) do
    {term, _binding} = Code.eval_string(string)
    term
  end

  def read_repos(config, repos_key \\ Hex.State.fetch!(:repos_key)) do
    hexpm = Hex.Repo.default_hexpm_repo(repos_key)

    (config[:"$repos"] || %{})
    |> Hex.Repo.merge_hexpm(hexpm)
    |> Hex.Repo.update_organizations()
  end

  def update_repos(repos) do
    config_repos =
      repos
      |> Hex.Repo.clean_organizations()
      |> Hex.Repo.clean_hexpm()

    state_repos =
      repos
      |> Hex.Repo.merge_hexpm()
      |> Hex.Repo.update_organizations()

    Hex.Config.update([{:"$repos", config_repos}])
    Hex.State.put(:repos, state_repos)
  end
end
