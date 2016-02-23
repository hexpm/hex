defmodule Hex.Registry do
  @name     __MODULE__
  @versions [3, 4]
  @filename "registry.ets"
  @pdict_id :"$hex_registry"
  @timeout  60_000

  def start_link do
    Agent.start_link(fn -> nil end, name: @name)
  end

  def open(opts \\ []) do
    Agent.get_and_update(@name, fn
      nil ->
        path = opts[:registry_path] || path()

        case :ets.file2tab(String.to_char_list(path)) do
          {:ok, tid} ->
            check_version(tid)
            {:ok, tid}

          {:error, reason} ->
            {{:error, reason}, nil}
        end

      tid ->
        {:ok, tid}
    end, @timeout)
  end

  def open!(opts \\ []) do
    case open(opts) do
      {:error, reason} ->
        Mix.raise "Failed to open Hex registry file (#{inspect reason})"
      _ ->
        :ok
    end
  end

  def close do
    Process.delete(@pdict_id)
    Agent.get_and_update(@name, fn
      nil ->
        {false, nil}
      tid ->
        :ets.delete(tid)
        {true, nil}
    end, @timeout)
  end

  def path do
    Path.join(Hex.State.fetch!(:home), @filename)
  end

  def clean_pdict do
    Process.delete(@pdict_id)
  end

  def info_installs do
    case :ets.lookup(get_tid(), :"$$installs2$$") do
      [{:"$$installs2$$", installs}] ->
        if version = latest_version(installs) do
          Hex.Shell.warn "A new Hex version is available (#{version}), " <>
                         "please update with `mix local.hex`"
        else
          check_elixir_version(installs)
        end
      _ ->
        :ok
    end
  end

  def stat do
    fun = fn
      {{package, version}, _}, {packages, releases}
          when is_binary(package) and is_binary(version) ->
        {packages, releases + 1}
      {package, _}, {packages, releases} when is_binary(package) ->
        {packages + 1, releases}
      _, acc ->
        acc
    end

    :ets.foldl(fun, {0, 0}, get_tid())
  end

  def search(term) do
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

    :ets.foldl(fun, [], get_tid())
    |> Enum.reverse
    |> Enum.sort
  end

  def exists?(package) do
    !! get_versions(package)
  end

  def exists?(package, version) do
    versions = get_versions(package)
    !! (versions && version in versions)
  end

  def get_versions(package) do
    case :ets.lookup(get_tid(), package) do
      [] -> nil
      [{^package, [versions|_]}] when is_list(versions) -> versions
    end
  end

  def get_deps(package, version) do
    case :ets.lookup(get_tid(), {package, version}) do
      [] ->
        nil
      [{{^package, ^version}, [deps|_]}] when is_list(deps) ->
        Enum.map(deps, fn
          [name, req, optional, app | _] -> {name, app, req, optional}
        end)
    end
  end

  def get_checksum(package, version) do
    case :ets.lookup(get_tid(), {package, version}) do
      [] ->
        nil
      [{{^package, ^version}, [_, checksum | _]}] when is_nil(checksum) or is_binary(checksum) ->
        checksum
    end
  end

  def get_build_tools(package, version) do
    case :ets.lookup(get_tid(), {package, version}) do
      [] ->
        nil
      [{{^package, ^version}, [_, _, build_tools | _]}] when is_list(build_tools) ->
        build_tools
    end
  end

  defp get_tid do
    if tid = Process.get(@pdict_id) do
      tid
    else
      tid = Agent.get(@name, & &1, @timeout)
      Process.put(@pdict_id, tid)
      tid
    end
  end

  defp check_version(tid) do
    case :ets.lookup(tid, :"$$version$$") do
      [{:"$$version$$", version}] when version in @versions ->
        :ok
      _ ->
        raise Mix.Error,
          message: "The registry file version is not supported. " <>
                   "Try updating Hex with `mix local.hex`."
    end
  end

  defp latest_version(versions) do
    current_elixir = System.version
    current_hex    = Hex.version

    versions
    |> Enum.filter(fn {hex, _} -> Hex.Version.compare(hex, current_hex) == :gt end)
    |> Enum.filter(fn {_, elixirs} -> Hex.Version.compare(hd(elixirs), current_elixir) != :gt end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sort(&(Hex.Version.compare(&1, &2) == :gt))
    |> List.first
  end

  defp check_elixir_version(versions) do
    {:ok, built}   = Hex.Version.parse(Hex.elixir_version())
    {:ok, current} = Hex.Version.parse(System.version)

    if match_minor?(current, built) do
      case :lists.keyfind(Hex.version, 1, versions) do
        {_, elixirs} ->
          if match_elixir_version?(elixirs, current) do
            Hex.Shell.warn "Hex was built against against Elixir #{Hex.elixir_version} " <>
              "and you are running #{System.version}, please run `mix local.hex` " <>
              "to update to a matching version"
          end

        false ->
          :ok
      end
    end
  end

  defp match_elixir_version?(elixirs, current) do
    Enum.any?(elixirs, fn elixir ->
      {:ok, elixir} = Hex.Version.parse(elixir)
      elixir.major == current.major and elixir.minor == current.minor
    end)
  end

  defp match_minor?(current, %Version{major: major, minor: minor}) do
    lower = %Version{major: major, minor: minor,     patch: 0, pre: [],  build: nil}
    upper = %Version{major: major, minor: minor + 1, patch: 0, pre: [0], build: nil}

    Hex.Version.compare(current, lower) in [:gt, :eq] and
      Hex.Version.compare(current, upper) == :lt
  end
end
