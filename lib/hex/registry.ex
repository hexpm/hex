defmodule Hex.Registry do
  @pdict_id :"$hex_registry"

  @type name :: term
  @type package :: String.t
  @type version :: String.t
  @callback open(Keyword.t) :: {:ok, name} | {:already_open, name} | {:error, String.t}
  @callback close(name) :: boolean
  @callback prefetch(name, package) :: :ok
  @callback versions(name, package) :: [version]
  @callback deps(name, package, version) :: [{String.t, String.t, String.t, boolean}]
  @callback checksum(name, package, version) :: binary

  options = quote do [
    prefetch(packages),
    versions(package),
    deps(package, version),
    checksum(package, version),
    tarball_etag(package, version),
    tarball_etag(package, version, etag)]
  end

  Enum.each(options, fn {function, _, args} ->
    def unquote(function)(unquote_splicing(args)) do
      {module, name} = pdict_get()
      module.unquote(function)(name, unquote_splicing(args))
    end
  end)

  def  pdict_clean,               do: Process.delete(@pdict_id)
  defp pdict_setup(module, name), do: Process.put(@pdict_id, {module, name})
  defp pdict_get,                 do: Process.get(@pdict_id)

  def open(module, opts \\ []) do
    case module.open(opts) do
      {ok, name} when ok in [:ok, :already_open] ->
        pdict_setup(module, name)
        ok
      {:error, reason} ->
        {:error, reason}
    end
  end

  def open!(module, opts \\ []) do
    case module.open(opts) do
      {ok, name} when ok in [:ok, :already_open] ->
        pdict_setup(module, name)
        ok
      {:error, reason} ->
        Mix.raise "Failed to open Hex registry (#{reason})"
    end
  end

  def close do
    case pdict_get() do
      {module, name} ->
        result = module.close(name)
        pdict_clean()
        result
      nil ->
        false
    end
  end
end
