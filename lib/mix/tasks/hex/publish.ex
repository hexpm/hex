defmodule Mix.Tasks.Hex.Publish do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

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

    * `:app` - Package name (required)

    * `:version` - Package version (required)

    * `:deps` - List of package dependencies (see Dependencies below)

    * `:description` - Description of the project in a few paragraphs

    * `:package` - Hex specific configuration (see Package configuration below)

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
       name

    * `:files` - List of files and directories to include in the package,
      can include wildcards

    * `:contributors` - List of names of contributors

    * `:licenses` - List of licenses used by the package

    * `:links` - Map of links
  """

  @switches [revert: :string, progress: :boolean]

  @default_files ~w(lib priv mix.exs README* readme* LICENSE*
                    license* CHANGELOG* changelog* src)

  @warn_fields ~w(description licenses contributors links)a
  @meta_fields @warn_fields ++ ~w(files name)a

  def run(args) do
    Hex.start
    Hex.Util.ensure_registry(fetch: false)

    {opts, _, _} = OptionParser.parse(args, switches: @switches)
    auth         = Util.auth_info()

    Mix.Project.get!
    config = Mix.Project.config

    {deps, exclude_deps} = dependencies(config)
    package = package(config)

    meta = Keyword.take(config, [:app, :version, :elixir, :description])
           |> Enum.into(%{})
           |> Map.put(:requirements, deps)
           |> Map.merge(package)

    if Mix.Project.umbrella?(config) do
      Mix.raise "Hex does not support umbrella projects"
    end

    if version = opts[:revert] do
      revert(meta, version, auth)
    else

      print_info(meta, exclude_deps)

      if Mix.shell.yes?("Proceed?") and create_package?(meta, auth) do
        progress? = Keyword.get(opts, :progress, true)
        create_release(meta, auth, progress?)
      end
    end
  end

  defp print_info(meta, exclude_deps) do
    Mix.shell.info("Publishing #{meta[:name]} v#{meta[:version]}")

    if meta[:requirements] != [] do
      Mix.shell.info("  Dependencies:")
      Enum.each(meta[:requirements], fn {app, %{requirement: req, optional: opt}} ->
        message = "    #{app} #{req}"
        if opt, do: message = message <> " (optional)"
        Mix.shell.info(message)
      end)
    end

    if exclude_deps != [] do
      Mix.shell.info("  Excluded dependencies (not part of the Hex package):")
      Enum.each(exclude_deps, &Mix.shell.info("    #{&1}"))
    end

    if meta[:files] != [] do
      Mix.shell.info("  Included files:")
      Enum.each(meta[:files], &Mix.shell.info("    #{&1}"))
    else
      Mix.shell.info("  WARNING! No included files")
    end

    fields = Dict.take(meta, @warn_fields) |> Dict.keys
    missing = @warn_fields -- fields

    if missing != [] do
      missing = Enum.join(missing, ", ")
      Mix.shell.info("  WARNING! Missing metadata fields: #{missing}")
    end
  end

  defp revert(meta, version, auth) do
    version = Util.clean_version(version)

    case Hex.API.Release.delete(meta[:name], version, auth) do
      {204, _} ->
        Mix.shell.info("Reverted #{meta[:name]} v#{version}")
      {code, body} ->
        Mix.shell.error("Reverting #{meta[:name]} v#{version} failed! (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp create_package?(meta, auth) do
    case Hex.API.Package.new(meta[:name], meta, auth) do
      {code, _} when code in 200..299 ->
        true
      {code, body} ->
        Mix.shell.error("Updating package #{meta[:name]} failed (#{code})")
        Hex.Util.print_error_result(code, body)
        false
    end
  end

  defp create_release(meta, auth, progress?) do
    tarball = Hex.Tar.create(meta, meta[:files])

    if progress? do
      progress = Util.progress(byte_size(tarball))
    else
      progress = Util.progress(nil)
    end

    case Hex.API.Release.new(meta[:name], tarball, auth, progress) do
      {code, _} when code in 200..299 ->
        Mix.shell.info("")
        Mix.shell.info("Published at #{Hex.Util.hex_package_url(meta[:name], meta[:version])}")
        Mix.shell.info("Don't forget to upload your documentation with `mix hex.docs`")
      {code, body} ->
        Mix.shell.error("Pushing #{meta[:name]} v#{meta[:version]} failed (#{code})")
        Hex.Util.print_error_result(code, body)
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
      {name, %{app: app, requirement: req, optional: opts[:optional]}}
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

  defp package(config) do
    package = Enum.into(config[:package] || [], %{})

    if files = package[:files] || @default_files do
      files = expand_paths(files, File.cwd!)
      package = Map.put(package, :files, files)
    end

    if name = package[:name] || config[:app] do
      package = Map.put(package, :name, name)
    end

    Map.take(package, @meta_fields)
  end
end
