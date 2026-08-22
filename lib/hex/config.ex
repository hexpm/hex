defmodule Hex.Config do
  @moduledoc false

  def read() do
    transaction(&do_read/0)
  end

  defp do_read() do
    case File.read(config_path()) do
      {:ok, binary} ->
        case decode_term(binary) do
          {:ok, term} ->
            migrate(term)

          {:error, _} ->
            config = decode_elixir(binary)
            do_write(config)
            migrate(config)
        end

      {:error, _} ->
        []
    end
  end

  # OAuth token maps were historically persisted with string keys. We now use
  # atom keys consistently (matching the :mix_hex_cli_auth border), so migrate
  # any string-keyed token maps from older configs on read.
  defp migrate(config) do
    Enum.map(config, fn
      {:"$oauth_token", token} ->
        {:"$oauth_token", migrate_oauth_token(token)}

      {:"$repos", repos} when is_map(repos) ->
        repos =
          Map.new(repos, fn {name, repo} ->
            {name, migrate_repo_oauth_token(repo)}
          end)

        {:"$repos", repos}

      pair ->
        pair
    end)
  end

  defp migrate_repo_oauth_token(repo) when is_map(repo) do
    case repo do
      %{oauth_token: token} -> %{repo | oauth_token: migrate_oauth_token(token)}
      _ -> repo
    end
  end

  defp migrate_repo_oauth_token(repo), do: repo

  defp migrate_oauth_token(token) when is_map(token) do
    Map.new(token, fn
      {"access_token", value} -> {:access_token, value}
      {"refresh_token", value} -> {:refresh_token, value}
      {"expires_at", value} -> {:expires_at, value}
      pair -> pair
    end)
  end

  defp migrate_oauth_token(token), do: token

  def update(config) do
    transaction(fn ->
      do_read()
      |> Keyword.merge(config)
      |> do_write()
    end)
  end

  def remove(keys) do
    transaction(fn ->
      do_read()
      |> Keyword.drop(keys)
      |> do_write()
    end)
  end

  def write(config) do
    transaction(fn -> do_write(config) end)
  end

  # Reads and writes of the config file are serialized against each other, so a
  # read-merge-write cannot lose what another process wrote in between. Callers
  # run in separate processes (registry and tarball fetchers) and under
  # different :mix_hex_cli_auth locks, so this is the only lock they share.
  defp transaction(fun) do
    :global.trans({{__MODULE__, :config}, self()}, fun, [node()], :infinity)
  end

  defp do_write(config) do
    config = Enum.reject(config, fn {_key, value} -> is_nil(value) end)
    string = encode_term(config)

    path = config_path()
    dir = Path.dirname(path)
    new_dir? = not File.dir?(dir)
    File.mkdir_p!(dir)
    if new_dir?, do: chmod(dir, 0o700)

    File.write!(path, string)
    chmod(path, 0o600)

    config
  end

  # The config holds the OAuth access token and the refresh token that mints
  # more of them, so it is readable only by its owner. Filesystems without Unix
  # modes reject the call instead of applying one, which is not a reason to fail
  # the write.
  defp chmod(path, mode) do
    _ = File.chmod(path, mode)
    :ok
  end

  defp config_path() do
    Path.join(hex_home(), "hex.config")
  end

  defp hex_home() do
    state_pid = Process.whereis(Hex.State)

    if state_pid && state_pid != self() do
      Hex.State.fetch!(:config_home)
    else
      {_, config_home} = find_config_home(:user_config)
      config_home
    end
  end

  def find_config_home(setting) do
    cond do
      dir = System.get_env("HEX_HOME") ->
        {{:env, "HEX_HOME"}, dir}

      System.get_env("MIX_XDG") in ["1", "true"] ->
        {{:env, "MIX_XDG"}, :filename.basedir(setting, "hex", %{os: :linux})}

      true ->
        {:ok, Path.expand("~/.hex")}
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
    case :io.read(pid, ~c"") do
      {:ok, term} -> consult(pid, [term | acc], string)
      {:error, reason} -> {:error, reason}
      :eof -> {:ok, Enum.reverse(acc)}
    end
  end

  defp decode_elixir(string) do
    {term, _binding} = Code.eval_string(string)
    term
  end

  def read_repos(config) do
    hexpm = Hex.Repo.default_hexpm_repo()

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
