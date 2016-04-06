defmodule Hex.Registry.ETS do
  @behaviour Hex.Registry

  @name     __MODULE__
  @versions [3, 4]
  @filename "registry.ets"
  @timeout  60_000

  def start_link do
    Agent.start_link(fn -> nil end, name: @name)
  end

  def open(opts) do
    Agent.get_and_update(@name, fn
      nil ->
        path = opts[:registry_path] || path()
        case :ets.file2tab(String.to_char_list(path)) do
          {:ok, tid} ->
            check_version(tid)
            {{:ok, tid}, tid}

          {:error, reason} ->
            if File.exists?(path) do
              {{:error, inspect(reason)}, nil}
            else
              {{:error, "file does not exist, run `mix hex.info` to fetch it"}, nil}
            end
        end

      tid ->
        {{:already_open, tid}, tid}
    end, @timeout)
  end

  def close do
    if tid = Agent.get(@name, & &1) do
      close(tid)
    else
      false
    end
  end

  def close(tid) do
    Agent.get_and_update(@name, fn
      nil ->
        {false, nil}
      agent_tid ->
        ^agent_tid = tid
        :ets.delete(tid)
        {true, nil}
    end, @timeout)
  end

  def memory do
    tid = Agent.get(@name, & &1)
    :ets.info(tid, :memory) * :erlang.system_info(:wordsize)
  end

  def path do
    Path.join(Hex.State.fetch!(:home), @filename)
  end

  def version(tid) do
    case :ets.lookup(tid, :"$$version$$") do
      [{:"$$version$$", version}] ->
        version
      _ ->
        nil
    end
  end

  def installs(tid) do
    case :ets.lookup(tid, :"$$installs2$$") do
      [{:"$$installs2$$", installs}] ->
        installs
      _ ->
        []
    end
  end

  def stat(tid) do
    fun = fn
      {{package, version}, _}, {packages, releases}
          when is_binary(package) and is_binary(version) ->
        {packages, releases + 1}
      {package, _}, {packages, releases} when is_binary(package) ->
        {packages + 1, releases}
      _, acc ->
        acc
    end

    :ets.foldl(fun, {0, 0}, tid)
  end

  def search(tid, term) do
    fun = fn
      {package, list}, packages when is_binary(package) and is_list(list) ->
        if String.contains?(package, term) do
          [package|packages]
        else
          packages
        end
      _, packages ->
        packages
    end

    :ets.foldl(fun, [], tid)
    |> Enum.sort
  end

  def all_packages(tid) do
    fun = fn
      {package, list}, packages when is_binary(package) and is_list(list) ->
        [package|packages]
      _, packages ->
        packages
    end

    :ets.foldl(fun, [], tid)
    |> Enum.sort
  end

  def get_versions(tid, package) do
    case :ets.lookup(tid, package) do
      [] -> nil
      [{^package, [versions|_]}] when is_list(versions) -> versions
    end
  end

  def get_deps(tid, package, version) do
    case :ets.lookup(tid, {package, version}) do
      [] ->
        nil
      [{{^package, ^version}, [deps|_]}] when is_list(deps) ->
        Enum.map(deps, fn
          [name, req, optional, app | _] -> {name, app, req, optional}
        end)
    end
  end

  def get_checksum(tid, package, version) do
    case :ets.lookup(tid, {package, version}) do
      [] ->
        nil
      [{{^package, ^version}, [_, checksum | _]}] when is_nil(checksum) or is_binary(checksum) ->
        checksum
    end
  end

  def get_build_tools(tid, package, version) do
    case :ets.lookup(tid, {package, version}) do
      [] ->
        nil
      [{{^package, ^version}, [_, _, build_tools | _]}] when is_list(build_tools) ->
        build_tools
    end
  end

  defp check_version(tid) do
    unless version(tid) in @versions do
      raise Mix.Error,
        message: "The registry file version is not supported. " <>
                 "Try updating Hex with `mix local.hex`."
    end
  end
end
