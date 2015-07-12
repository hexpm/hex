defmodule Mix.Tasks.Hex.Publish do
  use Mix.Task
  alias Mix.Tasks.Hex.Utils

  @shortdoc "Publish a new package version"

  @moduledoc """
  Publish a new version of your package and update the package.

  `mix hex.publish`

  If it is a new package being published it will be created and the user
  specified in `username` will be the package owner. Only package owners can
  publish.

  A published version can be amended or reverted with `--revert` up to one hour
  after its publication. If you want to revert a publication that is more than
  one hour old you need to contact an administrator.

  ## Command line options

    * `--revert VERSION` - Revert given version

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
        [ {:ecto, "~> 0.1.0"},
          {:postgrex, "~> 0.3.0"},
          {:cowboy, github: "extend/cowboy"} ]
      end

  As can be seen Hex package dependencies works alongside git dependencies.
  Important to note is that non-Hex dependencies will not be used during
  dependency resolution and neither will be they listed as dependencies of the
  package.

  ## Package configuration

  Additional metadata of the package can optionally be defined, but it is very
  recommended to do so.

    * `:name` - Set this if the package name is not the same as the application
       name.

    * `:files` - List of files and directories to include in the package,
      can include wildcards. Defaults to `["lib", "priv", "mix.exs", "README*",
      "readme*", "LICENSE*", "license*", "CHANGELOG*", "changelog*", "src"]`.

    * `:contributors` - List of names of contributors.

    * `:licenses` - List of licenses used by the package.

    * `:links` - Map of links relevant to the package.

    * `:build_tools` - List of build tools that can build the package. Hex will
      try to automatically detect the build tools, it will do this based on the
      files in the package. If a "rebar" or "rebar.config" file is present Hex
      will mark it as able to build with rebar. This detection can be overridden
      by setting this field.
  """

  @switches [revert: :string, progress: :boolean]

  @default_files ~w(lib priv mix.exs README* readme* LICENSE*
                    license* CHANGELOG* changelog* src)

  @error_fields ~w(files app name version build_tools)a
  @warn_fields ~w(description licenses contributors links)a
  @meta_fields @error_fields ++ @warn_fields ++ ~w(elixir)a

  def run(args) do
    Hex.start
    Hex.Utils.ensure_registry(update: false)

    {opts, _, _} = OptionParser.parse(args, switches: @switches)
    auth         = Utils.auth_info()

    Mix.Project.get!
    config = Mix.Project.config
    package = Enum.into(config[:package] || [], %{})

    {deps, exclude_deps} = dependencies(config)

    meta = Keyword.take(config, [:app, :version, :elixir, :description])
           |> Enum.into(%{})
           |> Map.merge(package)
           |> package(config)
           |> Map.put(:requirements, deps)

    if Mix.Project.umbrella?(config) do
      Mix.raise "Hex does not support umbrella projects"
    end

    if version = opts[:revert] do
      revert(meta, version, auth)
    else

      print_info(meta, exclude_deps)
      print_link_to_coc()

      if Hex.Shell.yes?("Proceed?") and create_package?(meta, auth) do
        progress? = Keyword.get(opts, :progress, true)
        create_release(meta, auth, progress?)
      end
    end
  end

  defp print_info(meta, exclude_deps) do
    Hex.Shell.info("Publishing #{meta[:name]} v#{meta[:version]}")

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
    error_missing(meta)

    if exclude_deps != [] do
      Hex.Shell.warn("  WARNING! Excluded dependencies (not part of the Hex package):")
      Enum.each(exclude_deps, &Hex.Shell.warn("    #{&1}"))
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

  defp print_link_to_coc() do
    Hex.Shell.info "Before publishing the package, make sure you've read " <>
      "the Hex Code of Conduct, available at https://hex.pm/docs/codeofconduct."
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

  defp warn_missing(meta) do
    missing(meta, @warn_fields, &Hex.Shell.warn("  WARNING! #{&1}"))
  end

  defp missing(meta, fields, printer) do
    taken_fields = Dict.take(meta, fields) |> Dict.keys
    missing = fields -- taken_fields

    if missing != [] do
      missing = Enum.join(missing, ", ")
      printer.("Missing metadata fields: #{missing}")
    end
  end

  defp revert(meta, version, auth) do
    version = Utils.clean_version(version)

    case Hex.API.Release.delete(meta[:name], version, auth) do
      {code, _} when code in 200..299 ->
        Hex.Shell.info("Reverted #{meta[:name]} v#{version}")
      {code, body} ->
        Hex.Shell.error("Reverting #{meta[:name]} v#{version} failed")
        Hex.Utils.print_error_result(code, body)
    end
  end

  # TODO: Remove
  defp create_package?(meta, auth) do
    case Hex.API.Package.new(meta[:name], meta, auth) do
      {code, _} when code in 200..299 ->
        true
      {code, body} ->
        Mix.shell.error("\nUpdating package #{meta[:name]} failed")
        Hex.Utils.print_error_result(code, body)
        false
    end
  end

  defp create_release(meta, auth, progress?) do
    tarball = Hex.Tar.create(meta, meta[:files])

    if progress? do
      progress = Utils.progress(byte_size(tarball))
    else
      progress = Utils.progress(nil)
    end

    case Hex.API.Release.new(meta[:name], tarball, auth, progress) do
      {code, _} when code in 200..299 ->
        Hex.Shell.info("\nPublished at #{Hex.Utils.hex_package_url(meta[:name], meta[:version])}")
        Hex.Shell.info("Don't forget to upload your documentation with `mix hex.docs`")
      {code, body} ->
        Hex.Shell.error("\nPushing #{meta[:name]} v#{meta[:version]} failed")
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp dependencies(meta) do
    deps = Enum.map(meta[:deps] || [], &Hex.Mix.dep/1)
    {include, exclude} = Enum.partition(deps, &(package_dep?(&1) and prod_dep?(&1)))

    Enum.each(include, fn {app, _req, opts} ->
      if opts[:override] do
        Mix.raise "Can't publish with overridden dependency #{app}, remove `override: true`"
      end
    end)

    include = for {app, req, opts} <- include, into: %{} do
      name = opts[:hex] || app
      {name, %{app: app, requirement: req, optional: opts[:optional] || false}}
    end
    exclude = for {app, _req, _opts} <- exclude, do: app
    {include, exclude}
  end

  defp prod_dep?({_app, _req, opts}) do
    if only = opts[:only], do: :prod in List.wrap(only), else: true
  end

  defp package_dep?({app, _req, opts}) do
    Enum.find(Mix.SCM.available, fn scm ->
      scm.accepts_options(app, opts)
    end) == Hex.SCM
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

  defp package(package, config) do
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
      if build_tool = guess_build_tool(files) do
        package = Map.put(package, :build_tools, [build_tool])
      end
    end

    Map.take(package, @meta_fields)
  end

  @build_tools [
    {"mix.exs"     , "mix"},
    {"rebar.config", "rebar"},
    {"rebar"       , "rebar"},
    {"Makefile"    , "make"},
    {"Makefile.win", "make"}
  ]

  defp guess_build_tool(paths) do
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
