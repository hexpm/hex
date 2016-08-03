defmodule Mix.Hex.Build do
  @default_files ~w(lib priv mix.exs README* readme* LICENSE*
                    license* CHANGELOG* changelog* src)

  @error_fields ~w(files app name description version build_tools)a
  @warn_fields ~w(licenses maintainers links)a
  @meta_fields @error_fields ++ @warn_fields ++ ~w(elixir extra)a
  @max_description_length 300

  def prepare_package! do
    Hex.start
    Hex.Utils.ensure_registry(fetch: false)
    Mix.Project.get!

    config = Mix.Project.config
    raise_if_umbrella_project!(config)

    package = Enum.into(config[:package] || [], %{})

    {deps, exclude_deps} = dependencies(config)

    meta = meta_for(config, package, deps)

    %{config: config, package: package, deps: deps,
      exclude_deps: exclude_deps, meta: meta}
  end

  def print_info(meta, exclude_deps, package_files) do
    if meta[:requirements] != [] do
      Hex.Shell.info("  Dependencies:")
      Enum.each(meta[:requirements], fn %{name: name, app: app, requirement: req, optional: opt} ->
        app = if name != app, do: " (app: #{app})"
        opt = if opt, do: " (optional)"
        message = "    #{name} #{req}#{app}#{opt}"
        Hex.Shell.info(message)
      end)
    end

    Enum.each(@meta_fields, &print_meta(meta, &1))

    errors =
      []
      |> error_missing!(meta)
      |> error_long_description(meta)
      |> error_missing_files(package_files)
      |> check_excluded_deps(exclude_deps)

    if errors != [] do
      error_msg =
        ["Stopping package build due to errors." | errors]
        |> Enum.join("\n")
      Mix.raise(error_msg)
    end
  end

  defp check_excluded_deps(errors, []), do: errors
  defp check_excluded_deps(errors, deps) do
    error = deps
      |> Enum.into(["Excluded dependencies (not part of the Hex package):"], &("    #{&1}"))
      |> Enum.join("\n")

    [error | errors]
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

      if opts[:compile] do
        Mix.raise "Can't build package when :compile is set for dependency #{app}, remove `compile: ...`"
      end

      if opts[:manager] do
        Mix.raise "Can't build package when :manager is set for dependency #{app}, remove `manager: ...`"
      end
    end)

    include =
      for {app, req, opts} <- include do
        name = opts[:hex] || app
        %{name: name, app: app, requirement: req, optional: opts[:optional] || false}
      end
    exclude = for {app, _req, _opts} <- exclude, do: app
    {include, exclude}
  end

  def package(package, config) do
    files = expand_paths(package[:files] || @default_files, File.cwd!)

    package
    |> Map.put(:files, files)
    |> maybe_put(:description, fn _ -> package[:description] end, &String.strip/1)
    |> maybe_put(:name, fn _ -> package[:name] || config[:app] end, & &1)
    |> maybe_put(:build_tools, fn _ -> !package[:build_tools] && guess_build_tools(files) end, & &1)
    |> Map.take(@meta_fields)
  end

  defp maybe_put(map, key, check, value) do
    if result = check.(map) do
      Map.put(map, key, value.(result))
    else
      map
    end
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
      Hex.Shell.error("No files")
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

  defp missing_files(nil), do: []
  defp missing_files(files) do
    Enum.filter(files, &(Path.wildcard(&1) == []))
  end

  defp error_missing!(errors, meta) do
    meta
    |> missing(@error_fields ++ @warn_fields)
    |> check_missing_fields(errors)
  end

  defp check_missing_fields([], errors), do: errors
  defp check_missing_fields(fields, errors) do
    fields = Enum.join(fields, ", ")
    ["Missing metadata fields: #{fields}" | errors]
  end

  defp error_long_description(errors, meta) do
    description = meta[:description] || ""

    if String.length(description) > @max_description_length do
      error_msg = ["Package description is very long (exceeds #{@max_description_length} characters)"]
      [error_msg | errors]
    else
      errors
    end
  end

  defp error_missing_files(errors, package_files) do
    package_files
    |> missing_files()
    |> check_missing_files(errors)
  end

  defp check_missing_files([], errors), do: errors
  defp check_missing_files(missing, errors) do
    missing = Enum.join(missing, ", ")
    ["Missing files: #{missing}" | errors]
  end

  defp missing(meta, fields) do
    taken_fields = Map.take(meta, fields) |> Map.keys
    fields -- taken_fields
  end

  @build_tools [
    {"mix.exs", "mix"},
    {"rebar", "rebar3"},
    {"rebar.lock", "rebar3"},
    {"rebar.config", "rebar3"},
    {"rebar.config.script", "rebar3"},
    {"erlang.mk", "make"},
    {"Makefile", "make"},
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
    |> default_build_tool
  end

  defp default_build_tool([]), do: ["mix"]
  defp default_build_tool(other), do: other
end
