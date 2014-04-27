defmodule Hex.Registry do
  @registry_tid :registry_tid
  @versions     [1, 2]

  def start(opts \\ []) do
    unless match?({ :ok, _ }, :application.get_env(:hex, @registry_tid)) do
      path = opts[:registry_path] || path()

      case :ets.file2tab(List.from_char_data!(path)) do
        { :ok, tid } ->
          :application.set_env(:hex, @registry_tid, tid)

          case :ets.lookup(tid, :"$$version$$") do
            [{ :"$$version$$", version }] when version in @versions ->
              :ok
            _ ->
              raise Mix.Error, message: "The registry file version is too new. Please update hex."
          end

        { :error, reason } ->
          raise Mix.Error, message: "Failed to open hex registry file (#{inspect reason})"
      end
    end
  end

  def stop do
    case :application.get_env(:hex, @registry_tid) do
      { :ok, tid } ->
        :ets.delete(tid)
        :application.unset_env(:hex, @registry_tid)
      :undefined ->
        :ok
    end
  end

  def path do
    Path.join(Mix.Utils.mix_home, "hex.ets")
  end

  def stat do
    fun = fn
      { { package, version }, _ }, { packages, releases }
          when is_binary(package) and is_binary(version) ->
        { packages, releases + 1 }
      { package, _ }, { packages, releases } when is_binary(package) ->
        { packages + 1, releases }
      _, acc ->
        acc
    end

    { :ok, tid } = :application.get_env(:hex, @registry_tid)
    :ets.foldl(fun, { 0, 0 }, tid)
  end

  def search(term) do
    fun = fn
      { package, list }, packages when is_binary(package) and is_list(list) ->
        if String.contains?(package, term) do
          [package|packages]
        else
          packages
        end
      _, packages ->
        packages
    end

    { :ok, tid } = :application.get_env(:hex, @registry_tid)
    :ets.foldl(fun, [], tid)
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
    { :ok, tid } = :application.get_env(:hex, @registry_tid)
    case :ets.lookup(tid, package) do
      [] -> nil
      [{ ^package, [versions|_] }] when is_list(versions) -> versions
      [{ ^package, versions }] -> versions
    end
  end

  def get_deps(package, version) do
    { :ok, tid } = :application.get_env(:hex, @registry_tid)
    case :ets.lookup(tid, { package, version }) do
      [] -> nil
      [{{^package, ^version}, [deps|_]}] when is_list(deps) -> deps
      [{{^package, ^version}, deps}] -> deps
    end
  end
end
