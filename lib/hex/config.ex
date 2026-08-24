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
    File.mkdir_p!(dir)

    # Unconditionally, not only when the directory is new. An install that
    # predates this is still world-readable, and the file mode below is no help
    # if anyone can list the directory and open what is in it.
    File.chmod!(dir, 0o700)

    write_private!(path, string)

    config
  end

  # The config holds the OAuth access token and the refresh token that mints
  # more of them, so no other user gets to read it. The mode is set on the
  # temporary file while it is still empty, so the tokens are never on disk under
  # the mode the umask picked, and the rename is atomic, so a reader gets either
  # the whole previous config or the whole new one.
  defp write_private!(path, string) do
    tmp_path = path <> "." <> Base.encode16(:crypto.strong_rand_bytes(8), case: :lower)

    try do
      create_private!(tmp_path)
      File.write!(tmp_path, string)
      File.rename!(tmp_path, path)
    rescue
      exception ->
        File.rm(tmp_path)
        reraise exception, __STACKTRACE__
    end
  end

  defp create_private!(path) do
    # The file exists at whatever mode the umask picked between the open and the
    # chmod. Nobody can reach it in that window, because the directory is 0700
    # by the time this runs, and the mode is set before anything is written to
    # it either way.
    case :file.open(path, [:write, :binary, :exclusive]) do
      {:ok, device} ->
        :ok = :file.close(device)
        File.chmod!(path, 0o600)

      {:error, reason} ->
        raise File.Error, reason: reason, action: "write to file", path: path
    end
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
    transaction(fn ->
      write_repos(do_read(), repos)
      put_state_repos(repos)
    end)
  end

  # Replaces one repository's config and keeps what another process wrote for
  # the others. A write goes through the whole $repos key, so two repositories
  # exchanging a token at the same time drop each other's unless the read
  # happens under the same lock as the write.
  #
  # The two halves start from different places on purpose. The file is what
  # another process may have written since, so the disk write starts from the
  # file. Memory holds repositories the file does not, set from HEX_REPOS or
  # HEX_MIRROR or by a caller, so the state write starts from the state. Taking
  # both from the file replaces those with whatever the file says the next time
  # any repository exchanges a token.
  def update_repo(name, fun) when is_function(fun, 1) do
    transaction(fn ->
      config = do_read()
      write_repos(config, apply_to_repo(read_repos(config), name, fun))
      put_state_repos(apply_to_repo(Hex.State.fetch!(:repos), name, fun))
    end)
  end

  defp apply_to_repo(repos, name, fun) do
    repo = Map.get_lazy(repos, name, fn -> Hex.Repo.get_repo(name) end)
    Map.put(repos, name, fun.(repo))
  end

  defp write_repos(config, repos) do
    config_repos =
      repos
      |> Hex.Repo.clean_organizations()
      |> Hex.Repo.clean_hexpm()

    config
    |> Keyword.put(:"$repos", config_repos)
    |> do_write()

    :ok
  end

  defp put_state_repos(repos) do
    state_repos =
      repos
      |> Hex.Repo.merge_hexpm()
      |> Hex.Repo.update_organizations()

    Hex.State.put(:repos, state_repos)
  end
end
