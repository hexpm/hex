defmodule Mix.Tasks.Hex.Build do
  use Mix.Task

  @default_files ~w(lib priv mix.exs README* readme* LICENSE*
                    license* CHANGELOG* changelog* src)
  @error_fields ~w(app name files description version build_tools)a
  @warn_fields ~w(licenses maintainers links)a
  @meta_fields @error_fields ++ @warn_fields ++ ~w(elixir extra)a
  @root_fields ~w(app version elixir description)a
  @max_description_length 300
  @default_repo "hexpm"
  @metadata_config "hex_metadata.config"
  @shortdoc "Builds a new package version locally"

  @moduledoc """
  Builds a new local version of your package.

  The package .tar file is created in the current directory, but is not pushed
  to the repository. An app named `foo` at version `1.2.3` will be built as
  `foo-1.2.3.tar`.

      mix hex.build

  ## Configuration

    * `:app` - Package name (required).
    * `:version` - Package version (required).
    * `:deps` - List of package dependencies (see Dependencies below).
    * `:description` - Short description of the project.
    * `:package` - Hex specific configuration (see Package configuration below).

  ## Dependencies

  Dependencies are defined in mix's dependency format. But instead of using
  `:git` or `:path` as the SCM `:package` is used.

      defp deps do
        [
          {:ecto, "~> 0.1.0"},
          {:postgrex, "~> 0.3.0"},
          {:cowboy, github: "extend/cowboy"}
        ]
      end

  As can be seen Hex package dependencies works alongside git dependencies.
  Important to note is that non-Hex dependencies will not be used during
  dependency resolution and neither will be they listed as dependencies of the
  package.

  ## Package configuration

  Additional package metadata is optional, but highly recommended.

    * `:name` - Set this if the package name is not the same as the application
       name.
    * `:files` - List of files and directories to include in the package,
      can include wildcards. Defaults to `["lib", "priv", "mix.exs", "README*",
      "readme*", "LICENSE*", "license*", "CHANGELOG*", "changelog*", "src"]`.
    * `:maintainers` - List of names and/or emails of maintainers.
    * `:licenses` - List of licenses used by the package.
    * `:links` - Map of links relevant to the package.
    * `:build_tools` - List of build tools that can build the package. Hex will
      try to automatically detect the build tools based on the files in the
      package. If a "rebar" or "rebar.config" file is present Hex will mark it
      as able to build with rebar. This detection can be overridden by setting
      this field.
  """

  def run(_args) do
    Hex.start()
    build = prepare_package()

    organization = build.organization
    meta = build.meta
    package = build.package
    exclude_deps = build.exclude_deps

    Hex.Shell.info("Building #{meta.name} #{meta.version}")
    print_info(meta, organization, exclude_deps, package[:files])

    {_tar, checksum} = Hex.Tar.create(meta, meta.files, false)
    Hex.Shell.info("Package checksum: #{checksum}")
  end

  def prepare_package() do
    Mix.Project.get!()
    config = Mix.Project.config()
    check_umbrella_project!(config)
    check_root_fields!(config)

    package = Enum.into(config[:package] || [], %{})
    check_misspellings!(package)
    {organization, package} = Map.pop(package, :organization)
    {deps, exclude_deps} = dependencies()
    meta = meta_for(config, package, deps)
    check_unstable_dependencies!(organization, meta)

    %{
      config: config,
      package: package,
      deps: deps,
      exclude_deps: exclude_deps,
      meta: meta,
      organization: organization,
    }
  end

  def print_info(meta, organization, exclude_deps, package_files) do
    if meta[:requirements] != [] do
      Hex.Shell.info("  Dependencies:")
      Enum.each(meta[:requirements], fn %{name: name, app: app, requirement: req, optional: opt, repository: repo} ->
        app = if name != app, do: " (app: #{app})"
        opt = if opt, do: " (optional)"
        repo = if repo != @default_repo, do: " (repo: #{repo})"
        message = "    #{name} #{req}#{app}#{repo}#{opt}"
        Hex.Shell.info(message)
      end)
    end

    if organization do
      Hex.Shell.info("  Organization: #{organization}")
    end

    Enum.each(@meta_fields, &print_metadata(meta, &1))

    errors =
      check_missing_fields(meta) ++
      check_description_length(meta) ++
      check_missing_files(package_files || []) ++
      check_reserved_files(package_files || []) ++
      check_excluded_deps(exclude_deps)

    if errors != [] do
      ["Stopping package build due to errors." | errors]
      |> Enum.join("\n")
      |> Mix.raise()
    end
  end

  defp check_excluded_deps([]), do: []
  defp check_excluded_deps(deps) do
    ["Dependencies excluded from the package (only Hex packages can be dependencies): #{Enum.join(deps, ", ")}"]
  end

  defp meta_for(config, package, deps) do
    config
    |> Keyword.take(@root_fields)
    |> Enum.into(%{})
    |> Map.merge(package)
    |> package(config)
    |> Map.put(:requirements, deps)
  end

  defp dependencies() do
    {include, exclude} =
      Mix.Dep.loaded([])
      |> Enum.filter(& &1.top_level)
      |> Enum.filter(&prod_dep?/1)
      |> Hex.enum_split_with(&package_dep?/1)

    Enum.each(include, fn %Mix.Dep{app: app, opts: opts} ->
      if opts[:override] do
        Mix.raise "Can't build package with overridden dependency #{app}, remove `override: true`"
      end

      if opts[:compile] do
        Mix.raise "Can't build package when :compile is set for dependency #{app}, remove `compile: ...`"
      end

      if opts[:manager] do
        Mix.raise "Can't build package when :manager is set for dependency #{app}, remove `manager: ...`"
      end

      if opts[:app] do
        Mix.raise "Can't build package when :app is set for dependency #{app}, remove `app: ...`"
      end
    end)

    include =
      Enum.map(include, fn %Mix.Dep{app: app, requirement: req, opts: opts} ->
        name = opts[:hex] || app
        repo = deorg_repo(opts[:repo] || @default_repo)
        %{name: name, app: app, requirement: req, optional: opts[:optional] || false, repository: repo}
      end)

    exclude = Enum.map(exclude, & &1.app)
    {include, exclude}
  end

  defp deorg_repo(repo) do
    case String.split(repo, ":", parts: 2) do
      [_source, repo] -> repo
      [repo] -> repo
    end
  end

  def package(package, config) do
    files = expand_paths(package[:files] || @default_files, File.cwd!)

    package
    |> Map.put(:files, files)
    |> maybe_put(:description, package[:description], &Hex.string_trim/1)
    |> maybe_put(:name, package[:name] || config[:app], &to_string(&1))
    |> maybe_put(:build_tools, !package[:build_tools] && guess_build_tools(files), &(&1))
    |> Map.take(@meta_fields)
  end

  defp maybe_put(map, key, value, transform) do
    if value do
      Map.put(map, key, transform.(value))
    else
      map
    end
  end

  def check_umbrella_project!(config) do
    if Mix.Project.umbrella?(config) do
      Mix.raise "Hex does not support umbrella projects"
    end
  end

  defp check_unstable_dependencies!(organization, meta) do
    if organization in [nil, "hexpm"] and not pre_requirement?(meta.version) and has_pre_requirements?(meta) do
      Mix.raise "A stable package release cannot have a pre-release dependency"
    end
  end

  defp check_misspellings!(opts) do
    if opts[:organisation] do
      Mix.raise "Invalid Hex package config :organisation, use spelling :organization"
    end
  end

  defp check_root_fields!(config) do
    package_only_fields = [:organisation, :organization] ++ @meta_fields -- @root_fields
    config_keys = Keyword.keys(config)
    invalid_field = Enum.find(config_keys, &(&1 in package_only_fields))

    if invalid_field do
      Hex.Shell.warn "Mix configuration #{inspect invalid_field} also belongs under the :package key, did you misplace it?"
    end
  end

  defp pre_requirement?(version_req) do
    String.contains?(version_req, "-")
  end

  defp has_pre_requirements?(meta) do
    meta.requirements
    |> Enum.map(& &1.requirement)
    |> Enum.any?(&pre_requirement?/1)
  end

  defp package_dep?(%Mix.Dep{scm: scm}) do
    scm == Hex.SCM
  end

  defp prod_dep?(%Mix.Dep{opts: opts}) do
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
    |> Enum.uniq()
    |> Enum.map(&Path.relative_to(&1, expand_dir))
  end

  defp dir_files(path) do
    if File.dir?(path) do
      Path.wildcard(Path.join(path, "**"))
    else
      [path]
    end
  end

  defp print_metadata(metadata, :files) do
    case metadata[:files] do
      [] ->
        Hex.Shell.error("No files")
      files ->
        Hex.Shell.info("  Files:")
        Enum.each(files, &Hex.Shell.info("    #{&1}"))
    end
  end

  defp print_metadata(metadata, key) do
    if value = metadata[key] do
      key =
        key
        |> Atom.to_string()
        |> String.replace("_", " ")
        |> String.capitalize()
      value = format_metadata_value(value)
      Hex.Shell.info("  #{key}: #{value}")
    end
  end

  defp format_metadata_value(list) when is_list(list) do
    Enum.join(list, ", ")
  end
  defp format_metadata_value(map) when is_map(map) do
    "\n    " <> Enum.map_join(map, "\n    ", fn {key, val} -> "#{key}: #{val}" end)
  end
  defp format_metadata_value(value) do
    value
  end

  defp check_missing_fields(metadata) do
    fields = @error_fields ++ @warn_fields
    taken_fields = Map.take(metadata, fields) |> Map.keys()

    case fields -- taken_fields do
      [] ->
        []
      missing ->
        ["Missing metadata fields: #{Enum.join(missing, ", ")}"]
    end
  end

  defp check_description_length(metadata) do
    descr = metadata[:description] || ""

    if String.length(descr) > @max_description_length do
      ["Package description is very long (exceeds #{@max_description_length} characters)"]
    else
      []
    end
  end

  defp check_missing_files(package_files) do
    case Enum.filter(package_files, &(Path.wildcard(&1) == [])) do
      [] ->
        []
      missing ->
        ["Missing files: #{Enum.join(missing, ", ")}"]
    end
  end

  defp check_reserved_files(package_files) do
    reserved_file = @metadata_config
    invalid_file = Enum.find(package_files, &(reserved_file in Path.wildcard(&1)))

    if invalid_file do
      ["Do not include this file: #{reserved_file}"]
    else
      []
    end
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
      |> Enum.into(Hex.Set.new)

    for {file, tool} <- @build_tools, file in base_files do
      tool
    end
    |> default_build_tool()
    |> Enum.uniq()
  end

  defp default_build_tool([]), do: ["mix"]
  defp default_build_tool(other), do: other
end
