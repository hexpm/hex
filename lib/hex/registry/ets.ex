defmodule Hex.Registry.ETS do
  @behaviour Hex.PackageRegistry

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
        {{:ok, tid}, tid}
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

  def path do
    Path.join(Hex.State.fetch!(:home), @filename)
  end

  def version(tid) do
    case :ets.lookup(tid, :"$$version$$") do
      [{:"$$version$$", version}] ->
        {:ok, version}
      _ ->
        {:ok, nil}
    end
  end

  def installs(tid) do
    case :ets.lookup(tid, :"$$installs2$$") do
      [{:"$$installs2$$", installs}] ->
        {:ok, installs}
      _ ->
        {:ok, []}
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

    {:ok, :ets.foldl(fun, {0, 0}, tid)}
  end

  def search(tid, term) do
    fun = fn
      {package, list}, packages when is_binary(package) and is_list(list) ->
        if String.contains?(package, term) do
          {:ok, [package|packages]}
        else
          {:ok, packages}
        end
      _, packages ->
        {:ok, packages}
    end

    {:ok, :ets.foldl(fun, [], tid) |> Enum.sort}
  end

  def all_packages(tid) do
    fun = fn
      {package, list}, packages when is_binary(package) and is_list(list) ->
        {:ok, [package|packages]}
      _, packages ->
        {:ok, packages}
    end

    {:ok, :ets.foldl(fun, [], tid) |> Enum.sort}
  end

  def get_versions(tid, package) do
    case :ets.lookup(tid, package) do
      [] -> {:ok, nil}
      [{^package, [versions|_]}] when is_list(versions) -> {:ok, versions}
    end
  end

  def get_deps(tid, package, version) do
    case :ets.lookup(tid, {package, version}) do
      [] ->
        {:ok, nil}
      [{{^package, ^version}, [deps|_]}] when is_list(deps) ->
        {:ok, Enum.map(deps, fn
          [name, req, optional, app | _] -> {name, app, req, optional}
        end)}
    end
  end

  def get_checksum(tid, package, version) do
    case :ets.lookup(tid, {package, version}) do
      [] ->
        {:ok, nil}
      [{{^package, ^version}, [_, checksum | _]}] when is_nil(checksum) or is_binary(checksum) ->
        {:ok, checksum}
    end
  end

  def get_build_tools(tid, package, version) do
    case :ets.lookup(tid, {package, version}) do
      [] ->
        {:ok, nil}
      [{{^package, ^version}, [_, _, build_tools | _]}] when is_list(build_tools) ->
        {:ok, build_tools}
    end
  end

  def to_lock(_tid, {name, app, version}) do
    {:ok, {String.to_atom(app), {:hex, String.to_atom(name), version}}}
  end

  def from_lock(_tid, {app, {:hex, name, version}}) do
    {:ok, [{Atom.to_string(name), Atom.to_string(app), version}]}
  end
  def from_lock(_tid, _), do: {:ok, []}

  defp check_version(tid) do
    {:ok, vsn} = version(tid)
    unless vsn in @versions do
      raise Mix.Error,
        message: "The registry file version is not supported. " <>
                 "Try updating Hex with `mix local.hex`."
    end
  end
end
