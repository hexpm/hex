defmodule Mix.Hex.Build do
  @default_files ~w(lib priv mix.exs README* readme* LICENSE*
                    license* CHANGELOG* changelog* src)

  @error_fields ~w(files app name version build_tools)a
  @warn_fields ~w(description licenses contributors links)a
  @meta_fields @error_fields ++ @warn_fields ++ ~w(elixir)a

  def prepare_package! do
    Hex.start
    Hex.Utils.ensure_registry(update: false)
    Mix.Project.get!

    config = Mix.Project.config
    raise_if_umbrella_project!(config)

    package = Enum.into(config[:package] || [], %{})

    {deps, exclude_deps} = dependencies(config)

    meta = meta_for(config, package, deps)

    [config: config, package: package, deps: deps,
      exclude_deps: exclude_deps, meta: meta]
  end

  def print_info(meta, exclude_deps, package_files) do
    if meta[:requirements] != [] do
      Hex.Shell.info("  Dependencies:")
      Enum.each(meta[:requirements], fn {app, %{requirement: req, optional: opt}} ->
        message = "    #{app} #{req}"
        if opt, do: message = message <> " (optional)"
        Hex.Shell.info(message)
      end)
    end

    Enum.each(@meta_fields, &print_meta(meta, &1))

    warn_missing(meta)
    warn_missing_files(package_files)
    error_missing(meta)

    if exclude_deps != [] do
      Hex.Shell.warn("  WARNING! Excluded dependencies (not part of the Hex package):")
      Enum.each(exclude_deps, &Hex.Shell.warn("    #{&1}"))
    end
  end

  defp meta_for(config, package, deps) do
    Keyword.take(config, [:app, :version, :elixir, :description])
    |> Enum.into(%{})
    |> Map.merge(package)
    |> package(config)
    |> Map.put(:requirements, deps)
  end

  defp dependencies(meta) do
    deps = Enum.map(meta[:deps] || [], &Hex.Mix.dep/1)
    {include, exclude} = Enum.partition(deps, &(package_dep?(&1) and prod_dep?(&1)))

    Enum.each(include, fn {app, _req, opts} ->
      if opts[:override] do
        Mix.raise "Can't build package with overridden dependency #{app}, remove `override: true`"
      end
    end)

    include = for {app, req, opts} <- include, into: %{} do
      name = opts[:hex] || app
      {name, %{app: app, requirement: req, optional: opts[:optional] || false}}
    end
    exclude = for {app, _req, _opts} <- exclude, do: app
    {include, exclude}
  end

  def package(package, config) do
    if description = package[:description] do
      package = Map.put(package, :description, String.strip(description))
    end

    if files = package[:files] || @default_files do
      files = expand_paths(files, File.cwd!)
      package = Map.put(package, :files, files)
    end

    if name = package[:name] || config[:app] do
      package = Map.put(package, :name, name)
    end

    unless package[:build_tools] do
      if build_tools = guess_build_tools(files) do
        package = Map.put(package, :build_tools, build_tools)
      end
    end

    Map.take(package, @meta_fields)
  end

  def raise_if_umbrella_project!(config) do
    if Mix.Project.umbrella?(config) do
      Mix.raise "Hex does not support umbrella projects"
    end
  end

  defp package_dep?({app, _req, opts}) do
    Enum.find(Mix.SCM.available, fn scm ->
      scm.accepts_options(app, opts)
    end) == Hex.SCM
  end

  defp prod_dep?({_app, _req, opts}) do
    if only = opts[:only], do: :prod in List.wrap(only), else: true
  end

  defp expand_paths(paths, dir) do
    expand_dir = Path.expand(dir)

    paths
    |> Enum.map(&Path.join(dir, &1))
    |> Enum.flat_map(&Path.wildcard/1)
    |> Enum.flat_map(&dir_files/1)
    |> Enum.map(&Path.expand/1)
    |> Enum.filter(&File.regular?/1)
    |> Enum.uniq
    |> Enum.map(&Path.relative_to(&1, expand_dir))
  end

  defp dir_files(path) do
    if File.dir?(path) do
      Path.wildcard(Path.join(path, "**"))
    else
      [path]
    end
  end

  defp print_meta(meta, :files) do
    if meta[:files] != [] do
      Hex.Shell.info("  Files:")
      Enum.each(meta[:files], &Hex.Shell.info("    #{&1}"))
    else
      Hex.Shell.warn("  WARNING! No files")
    end
  end

  defp print_meta(meta, key) do
    if value = meta[key] do
      key = key |> Atom.to_string |> String.replace("_", " ") |> String.capitalize
      value = meta_value(value)
      Hex.Shell.info("  #{key}: #{value}")
    end
  end

  defp meta_value(list) when is_list(list),
    do: Enum.join(list, ", ")
  defp meta_value(map) when is_map(map),
    do: "\n    " <> Enum.map_join(map, "\n    ", fn {k, v} -> "#{k}: #{v}" end)
  defp meta_value(value),
    do: value

  defp error_missing(meta) do
    missing(meta, @error_fields, &Hex.Shell.error("  ERROR! #{&1}"))
  end

  defp missing_files(nil), do: []
  defp missing_files(files) do
    Enum.filter(files, &(Path.wildcard(&1) == []))
  end

  defp warn_missing(meta) do
    missing(meta, @warn_fields, &Hex.Shell.warn("  WARNING! #{&1}"))
  end

  defp warn_missing_files(package_files) do
    missing_files = missing_files(package_files)
    if missing_files != [] do
      missing = Enum.join(missing_files, ", ")
      Hex.Shell.warn("  WARNING! Missing files: #{missing}")
    end
  end

  defp missing(meta, fields, printer) do
    taken_fields = Map.take(meta, fields) |> Map.keys
    missing = fields -- taken_fields

    if missing != [] do
      missing = Enum.join(missing, ", ")
      printer.("Missing metadata fields: #{missing}")
    end
  end

  @build_tools [
    {"mix.exs"     , "mix"},
    {"rebar.config", "rebar"},
    {"rebar"       , "rebar"},
    {"Makefile"    , "make"},
    {"Makefile.win", "make"}
  ]

  defp guess_build_tools(paths) do
    base_files =
      paths
      |> Enum.filter(&(Path.dirname(&1) == "."))
      |> Enum.into(HashSet.new)

    Enum.flat_map(@build_tools, fn {file, tool} ->
      if file in base_files,
        do: [tool],
      else: []
    end)
  end
end
