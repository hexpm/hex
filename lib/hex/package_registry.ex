defmodule Hex.PackageRegistry do
  @name __MODULE__

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
  @callback to_lock(term, tuple)                      :: map
  @callback from_lock(term, map)                      :: [tuple]

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
      call_registry(unquote(function), [unquote_splicing(args)])
    end
  end)

  def pdict_clean do
    Agent.update(@name, &Enum.map(&1, fn {registry, _} -> {registry, nil} end))
  end

  def start_link do
    Agent.start_link(fn -> [] end, name: @name)
  end

  def append(registry) do
    Agent.update(@name, &(&1 ++ [{registry, nil}]))
  end

  def open!(opts \\ []) do
    case open(opts) do
      :ok -> :ok
      {:error, reason} ->
        Mix.raise "Failed to open Hex registry (#{reason})"
    end
  end

  def open(opts \\ []) do
    registries = Agent.get(@name, & &1)
    # TODO: close succeeded on a failure?
    {opened, failed} =
      registries
      |> Enum.map(fn {module, _name} -> {module, module.open(opts)} end)
      |> Enum.partition(fn
        {_, {:ok, _name}} -> true
        {_, {:error, _reason}} -> false
      end)

    registries =
      opened
      |> Enum.map(fn {module, {:ok, name}} -> {module, name} end)

    Agent.update(@name, fn _ -> registries end)
    if Enum.any?(failed) do
      {_, {:error, reason}} = List.first(failed)
      {:error, reason}
    else
      :ok
    end
  end

  def close do
    did_close =
      @name
      |> Agent.get(& &1)
      |> Enum.any?(fn {module, name} -> module.close(name) end)

    pdict_clean()
    did_close
  end

  @spec to_lock(%{}) :: %{}
  def to_lock(result) do
    items = Enum.map(result, &call_registry(:to_lock, [&1]))

    failure = Enum.find(items, fn
      {:error, _error} -> true
      _ -> false
    end)

    if failure do
      failure
    else
      Enum.into(items, %{})
    end
  end

  def from_lock(lock) do
    items = Enum.flat_map(lock, &call_registry(:from_lock, [&1]))

    failure = Enum.find(items, fn
      {:error, _error} -> true
      _ -> false
    end)

    if failure do
      failure
    else
      items
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

  defp call_registry(function, args) do
    result =
      @name
      |> Agent.get(& &1)
      |> Stream.map(fn {module, name} -> apply(module, function, [name | args]) end)
      |> Enum.find(fn
        {:ok, _result} -> true
        {:error, :no_package} -> false
        {:error, _reason} -> true
      end)

    case result do
      {:ok, value} -> value
      {:error, reason} -> Mix.raise reason
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
