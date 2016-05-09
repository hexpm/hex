defmodule Hex.Registry do
  @pdict_id :"$hex_registry"

  @callback open(Keyword.t)                           :: {:ok, term} | {:error, String.t}
  @callback close(term)                               :: boolean
  @callback version(term)                             :: String.t
  @callback installs(term)                            :: [{String.t, String.t}]
  @callback stat(term)                                :: {non_neg_integer, non_neg_integer}
  @callback search(term, String.t)                    :: [String.t]
  @callback all_packages(term)                        :: [String.t]
  @callback get_versions(term, String.t)              :: [String.t]
  @callback get_deps(term, String.t, String.t)        :: [{String.t, String.t, String.t, boolean}]
  @callback get_checksum(term, String.t, String.t)    :: binary
  @callback get_build_tools(term, String.t, String.t) :: [String.t]

  options = quote do [
    version(),
    installs(),
    stat(),
    search(term),
    all_packages(),
    get_versions(package),
    get_deps(package, version),
    get_checksum(package, version),
    get_build_tools(package, version)]
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

  def info_installs do
    installs = installs()
    if version = latest_version(installs) do
      Hex.Shell.warn "A new Hex version is available (#{version}), " <>
                     "please update with `mix local.hex`"
    else
      check_elixir_version(installs)
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

    unless match_minor?(current, built) do
      case :lists.keyfind(Hex.version, 1, versions) do
        {_, elixirs} ->
          if match_elixir_version?(elixirs, current) do
            Hex.Shell.warn "Hex was built against Elixir #{Hex.elixir_version} " <>
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
