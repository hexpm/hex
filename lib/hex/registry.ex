defmodule Hex.Registry do
  @registry_tid :registry_tid
  @version      1

  def start(opts \\ []) do
    path = opts[:registry_path] || path()

    case :ets.file2tab(String.to_char_list!(path)) do
      { :ok, tid } ->
        :application.set_env(:hex, @registry_tid, tid)

        case :ets.lookup(tid, :"$$version$$") do
          [{ :"$$version$$", @version }] ->
            :ok
          _ ->
            raise Hex.Error, message: "The registry file version is newer than what is supported. " <>
              "Please update hex."
        end

      { :error, reason } ->
        raise Hex.Error, message: "Failed to open hex registry file (#{inspect reason}). " <>
          "Did you fetch it with 'mix hex.update'?"
    end
  end

  def stop do
    { :ok, tid } = :application.get_env(:hex, @registry_tid)
    :ets.delete(tid)
    :ok
  end

  def path do
    Path.join(Mix.Utils.mix_home, "hex.ets")
  end

  def stat do
    fun = fn
      { { _, _ }, _ }, { packages, releases } ->
        { packages, releases + 1 }
      { binary, list }, { packages, releases }
          when is_binary(binary) and is_list(list) ->
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
      [{ ^package, versions }] -> versions
    end
  end

  def get_release(package, version) do
    { :ok, tid } = :application.get_env(:hex, @registry_tid)
    case :ets.lookup(tid, { package, version }) do
      [] -> nil
      [release] -> release
    end
  end
end
