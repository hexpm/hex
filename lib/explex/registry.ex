defmodule Explex.Registry do
  defrecord Package, [:key, :deps, :url, :ref]
  defrecord PackageVsn, [:key, :versions]

  @ets_table  :explex_ets_registry
  @dets_table :explex_dets_registry
  @version    1

  def start(opts \\ []) do
    filename = opts[:registry_path] || path()
    ram_file = opts[:ram_file] || false

    dets_opts = [
      access: :read,
      file: filename,
      ram_file: ram_file,
      type: :duplicate_bag ]

    ets_opts = [
      :set,
      :named_table,
      { :keypos, 2 },
      :public ]

    case :dets.open_file(@dets_table, dets_opts) do
      { :ok, @dets_table } ->
        :ok
      { :error, _reason } ->
        raise Explex.Error, message: "Failed to open explex registry file. " <>
          "Did you fetch it with 'mix explex.update'?"
    end
    :ets.new(@ets_table, ets_opts)

    case :dets.lookup(@dets_table, :"$$version$$") do
      [{ :"$$version$$", @version }] ->
        :ok
      _ ->
        raise Explex.Error, message: "The registry file version is newer than what is supported. " <>
          "Please update explex."
    end
  end

  def stop do
    :ets.delete(@ets_table)
    :dets.close(@dets_table)
    :ok
  end

  def path do
    Path.join(Mix.Utils.mix_home, "explex.dets")
  end

  def stat do
    fun = fn
      { name, _, _, _, _ }, { packages, releases } ->
        { Set.put(packages, name), releases + 1 }
      _, acc ->
        acc
    end
    { packages, releases } = :dets.foldl(fun, { HashSet.new, 0 }, @dets_table)
    { Set.size(packages), releases }
  end

  def package_exists?(package) do
    load_package?(package)
  end

  def get_versions(package) do
    case :ets.lookup(@ets_table, package) do
      [] ->
        if load_package?(package), do: get_versions(package)
      [PackageVsn[versions: versions]] ->
        versions
    end
  end

  def get_package(package, version) do
    case :ets.lookup(@ets_table, { package, version }) do
      [] ->
        if load_package?(package), do: get_package(package, version)
      [package] ->
        package
    end
  end

  def version_from_ref(package, url, ref) do
    match = Package[key: { package, :"$1" }, deps: :_, url: url, ref: ref]

    case :ets.match(@ets_table, match) do
      [] ->
        case load_package?(package) && :ets.match(@ets_table, match) do
          [[version]] -> { :ok, version }
          _ -> :error
        end
      [[version]] ->
        { :ok, version }
    end
  end

  defp load_package?(package) do
    packages = :dets.lookup(@dets_table, package)
    versions =
      Enum.map(packages, fn { name, version, deps, url, ref } ->
        package = Package[key: { name, version }, deps: deps, url: url, ref: ref]
        :ets.insert(@ets_table, package)
        version
      end)

    found? = versions != []
    if found? do
      versions = Enum.sort(versions, &(Version.compare(&1, &2) == :gt))
      package = PackageVsn[key: package, versions: versions]
      :ets.insert(@ets_table, package)
    end
    found?
  end
end
