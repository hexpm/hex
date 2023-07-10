defmodule Mix.Tasks.Hex.Build do
  use Mix.Task

  @shortdoc "Builds a new package version locally"

  @moduledoc """
  Builds a new local version of your package.

  The package .tar file is created in the current directory, but is not pushed
  to the repository. An app named `foo` at version `1.2.3` will be built as
  `foo-1.2.3.tar`.

      $ mix hex.build

  #{Hex.Package.configuration_doc()}

  > In a project, this task must be invoked before any other tasks
  > that may load or start your application. Otherwise, you must
  > explicitly list `:hex` as part of your `:extra_applications`.

  ### Command line options

    * `--unpack` - Builds the tarball and unpacks contents into a directory.
      Useful for making sure the tarball contains all needed files before
      publishing. See `--output` below for setting the output path.

    * `-o`, `--output` - Sets output path. When used with `--unpack` it means
      the directory (Default: `<app>-<version>`). Otherwise, it specifies
      tarball path (Default: `<app>-<version>.tar`)

  """
  @behaviour Hex.Mix.TaskDescription

  @error_fields ~w(app name files version build_tools)a
  @warn_fields ~w(description licenses links)a
  @meta_fields @error_fields ++ @warn_fields ++ ~w(elixir extra)a
  @root_fields ~w(app version elixir description)a
  @max_description_length 300
  @default_repo "hexpm"
  @metadata_config "hex_metadata.config"

  @switches [unpack: :boolean, output: :string]
  @aliases [o: :output]

  @impl true
  def run(args) do
    Hex.start()
    {opts, _args} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    build = prepare_package()

    organization = build.organization
    meta = build.meta
    package = build.package
    exclude_deps = build.exclude_deps

    Hex.Shell.info("Building #{meta.name} #{meta.version}")
    print_info(meta, organization, exclude_deps, package[:files])

    if opts[:unpack] do
      output = Keyword.get(opts, :output, "#{meta.name}-#{meta.version}")
      build_and_unpack_package(meta, output)
    else
      output = Keyword.get(opts, :output, "#{meta.name}-#{meta.version}.tar")
      build_package(meta, output)
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Builds a new package version locally"}
    ]
  end

  defp build_package(meta, output) do
    %{outer_checksum: outer_checksum} = Hex.Tar.create!(meta, meta.files, output)
    Hex.Shell.info("Package checksum: #{Base.encode16(outer_checksum, case: :lower)}")
    Hex.Shell.info("Saved to #{output}")
  end

  defp build_and_unpack_package(meta, output) do
    %{tarball: tarball, inner_checksum: inner_checksum, outer_checksum: outer_checksum} =
      Hex.Tar.create!(meta, meta.files, :memory)

    %{inner_checksum: ^inner_checksum, outer_checksum: ^outer_checksum} =
      Hex.Tar.unpack!({:binary, tarball}, output)

    Hex.Shell.info("Saved to #{output}")
  end

  @doc false
  def prepare_package() do
    Mix.Project.get!()
    config = Mix.Project.config()
    check_umbrella_project!(config)
    check_root_fields!(config)

    package = Map.new(config[:package] || [])
    check_misspellings!(package)
    {organization, package} = Map.pop(package, :organization)
    {deps, exclude_deps} = dependencies()
    meta = meta_for(config, package, deps)

    %{
      config: config,
      package: package,
      deps: deps,
      exclude_deps: exclude_deps,
      meta: meta,
      organization: organization
    }
  end

  @doc false
  def print_info(meta, organization, exclude_deps, package_files) do
    if meta[:requirements] != [] do
      Hex.Shell.info("  Dependencies:")

      Enum.each(meta[:requirements], fn requirement ->
        %{name: name, app: app, requirement: req, optional: opt, repository: repo} = requirement
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
      Enum.concat([
        check_missing_fields(meta, organization),
        check_description_length(meta),
        check_missing_files(package_files || []),
        check_reserved_files(package_files || []),
        check_excluded_deps(exclude_deps)
      ])

    if errors != [] do
      ["Stopping package build due to errors." | errors]
      |> Enum.join("\n")
      |> Mix.raise()
    end

    if organization in [nil, "hexpm"] do
      licenses_valid_or_warn(meta.licenses)
    end
  end

  defp licenses_valid_or_warn([]), do: Hex.Shell.warn("\nYou have not included any licenses\n")

  defp licenses_valid_or_warn(licenses) do
    invalid_licenses = Enum.reject(licenses, fn lic -> :mix_hex_licenses.valid(lic) end)

    if invalid_licenses != [] do
      message = [
        "The following licenses are not recognized by SPDX:\n",
        Enum.map(invalid_licenses, &" * #{&1}\n"),
        "\nConsider using licenses from https://spdx.org/licenses"
      ]

      Hex.Shell.warn(message)
    end
  end

  defp check_excluded_deps([]), do: []

  defp check_excluded_deps(deps) do
    [
      "Dependencies excluded from the package (only Hex packages can be dependencies): #{Enum.join(deps, ", ")}"
    ]
  end

  defp meta_for(config, package, deps) do
    config
    |> Keyword.take(@root_fields)
    |> Map.new()
    |> Map.merge(package)
    |> package(config)
    |> Map.put(:requirements, deps)
  end

  defp dependencies() do
    {include, exclude} =
      Mix.Project.config()[:deps]
      |> Enum.map(&Hex.Mix.normalize_dep/1)
      |> Enum.filter(&prod_dep?/1)
      |> Enum.split_with(&package_dep?/1)

    Enum.each(include, fn {app, _req, opts} ->
      if Keyword.has_key?(opts, :override) do
        Mix.raise(
          "Can't build package with overridden dependency #{app}, remove `override: true`"
        )
      end

      if Keyword.has_key?(opts, :compile) do
        Mix.raise(
          "Can't build package when :compile is set for dependency #{app}, remove `compile: ...`"
        )
      end

      if Keyword.has_key?(opts, :manager) do
        Mix.raise(
          "Can't build package when :manager is set for dependency #{app}, remove `manager: ...`"
        )
      end

      if Keyword.has_key?(opts, :app) do
        Mix.raise("Can't build package when :app is set for dependency #{app}, remove `app: ...`")
      end

      if Keyword.get(opts, :system_env, []) != [] do
        Mix.raise(
          "Can't build package when :system_env is set for dependency #{app}, remove `system_env: ...`"
        )
      end
    end)

    include =
      Enum.map(include, fn {app, req, opts} ->
        name = opts[:hex] || app
        repo = deorg_repo(opts[:repo] || opts[:organization] || @default_repo)

        %{
          name: to_string(name),
          app: app,
          requirement: req,
          optional: opts[:optional] || false,
          repository: repo
        }
      end)

    exclude = for {app, _req, _opts} <- exclude, do: app
    {include, exclude}
  end

  defp deorg_repo(repo) do
    case String.split(to_string(repo), ":", parts: 2) do
      [_source, repo] -> repo
      [repo] -> repo
    end
  end

  @doc false
  def package(package, config) do
    files = package[:files] || Hex.Package.default_files()
    exclude_patterns = (package[:exclude_patterns] || []) ++ [~r/\W\.DS_Store$/]

    files =
      files
      |> expand_paths(File.cwd!())
      |> Enum.reject(fn path ->
        Enum.any?(exclude_patterns, &(path =~ &1))
      end)

    package
    |> Map.put(:files, files)
    |> maybe_put(:description, package[:description], &String.trim/1)
    |> maybe_put(:name, package[:name] || config[:app], &to_string(&1))
    |> maybe_put(:build_tools, !package[:build_tools] && guess_build_tools(files), & &1)
    |> Map.take(@meta_fields)
  end

  defp maybe_put(map, key, value, transform) do
    if value do
      Map.put(map, key, transform.(value))
    else
      map
    end
  end

  @doc false
  def check_umbrella_project!(config) do
    if Mix.Project.umbrella?(config) do
      Mix.raise("Hex does not support umbrella projects")
    end
  end

  defp check_misspellings!(opts) do
    if opts[:organisation] do
      Mix.raise("Invalid Hex package config :organisation, use spelling :organization")
    end
  end

  defp check_root_fields!(config) do
    package_only_fields =
      ([:organisation, :organization] ++ @meta_fields) -- (@root_fields ++ [:name])

    config_keys = Keyword.keys(config)
    invalid_field = Enum.find(config_keys, &(&1 in package_only_fields))

    if invalid_field do
      Hex.Shell.warn(
        "Mix project configuration #{inspect(invalid_field)} belongs under the :package key, did you misplace it?"
      )
    end
  end

  @scm_keys [:git, :github, :path]

  defp package_dep?({_app, _req, opts}) do
    keys = Keyword.keys(opts)
    :hex in keys or not Enum.any?(@scm_keys, &(&1 in keys))
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
    |> Enum.uniq()
    |> Enum.map(&Path.relative_to(&1, expand_dir))
  end

  defp dir_files(path) do
    case File.lstat(path) do
      {:ok, %File.Stat{type: :directory}} ->
        new_paths =
          path
          |> File.ls!()
          |> Enum.map(&Path.join(path, &1))
          |> Enum.flat_map(&dir_files/1)

        [path | new_paths]

      _ ->
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

  defp check_missing_fields(metadata, organization) do
    if organization in [nil, "hexpm"] do
      check_error_fields(metadata, @error_fields ++ @warn_fields)
    else
      check_warn_fields(metadata, @warn_fields)
      check_error_fields(metadata, @error_fields)
    end
  end

  defp check_warn_fields(metadata, warn_fields) do
    case check_error_fields(metadata, warn_fields) do
      [message] -> Hex.Shell.warn(message)
      [] -> :ok
    end
  end

  defp check_error_fields(metadata, error_fields) do
    taken_fields = Map.take(metadata, error_fields) |> Map.keys()
    missing = error_fields -- taken_fields

    if missing == [] do
      []
    else
      ["Missing metadata fields: #{Enum.join(missing, ", ")}"]
    end
  end

  defp check_description_length(metadata) do
    descr = metadata[:description] || ""

    if String.length(descr) > @max_description_length do
      ["Package description is too long (exceeds #{@max_description_length} characters)"]
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
      |> MapSet.new()

    for {file, tool} <- @build_tools, file in base_files do
      tool
    end
    |> default_build_tool()
    |> Enum.uniq()
  end

  defp default_build_tool([]), do: ["mix"]
  defp default_build_tool(other), do: other
end
