defmodule Mix.Tasks.Hex.Publish do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Publish a new package version"

  @moduledoc """
  Publish a new version of your package and update the package.

  `mix hex.publish -u username -p password`

  If it is a new package being published it will be created and the user
  specified in `username` will be the package owner. Only package owners can
  publish.

  A published version can be amended or reverted with `--revert` up to one hour
  after its publication. If you want to revert a publication that is more than
  one hour old you need to contact an administrator.

  ## Command line options

  * `--user`, `-u` - Username of package owner (overrides user stored in config)

  * `--pass`, `-p` - Password of package owner (required if `--user` was given)

  * `--revert version` - Revert given version

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
        [ { :ecto, "~> 0.1.0" },
          { :postgrex, "~> 0.3.0" },
          { :cowboy, github: "extend/cowboy" } ]
      end

  As can be seen Hex package dependencies works alongside git dependencies.
  Important to note is that non-Hex dependencies will not be used during
  dependency resolution and neither will be they listed as dependencies of the
  package.

  ## Package configuration

  Additional metadata of the package can optionally be defined, but it is very
  recommended to do so.

  * `:files` - List of files and directories to include in the package

  * `:contributors` - List of names of contributors

  * `:licenses` - List of licenses used by the package

  * `:links` - Dictionary of links
  """

  @switches [revert: :string]
  @aliases [u: :user, p: :pass]

  @default_files [ "lib", "priv", "mix.exs", "README*", "readme*", "LICENSE*",
                   "license*", "CHANGELOG*", "changelog*", "src" ]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, switches: @switches, aliases: @aliases)
    user_config = Hex.Mix.read_config
    auth        = Util.auth_opts(opts, user_config)

    Mix.Project.get!
    Hex.start

    if version = opts[:revert] do
      revert(Mix.project, version, opts)
    else
      config = Mix.project
      { deps, exclude_deps } = dependencies(config)

      package = config[:package]
      files   = expand_paths(package[:files] || @default_files)
      meta    = Keyword.take(config, [:app, :version, :description])
      meta    = meta ++ [requirements: deps, files: files] ++ (package || [])

      print_info(meta, exclude_deps)

      if Mix.shell.yes?("Proceed?") and create_package?(meta, auth) do
        create_release(meta, auth)
      end
    end
  end

  defp print_info(meta, exclude_deps) do
    # TODO: Run dependency resolution (don't use mix.lock), check that deps
    # are valid packages and has valid version requirements

    Mix.shell.info("Publishing #{meta[:app]} v#{meta[:version]}")

    Mix.shell.info("  Dependencies:")
    Enum.each(meta[:requirements], fn
      { app, req } -> Mix.shell.info("    #{app} #{req}")
    end)

    Mix.shell.info("  Excluded dependencies (not part of the Hex package):")
    Enum.each(exclude_deps, &Mix.shell.info("    #{&1}"))

    Mix.shell.info("  Included files:")
    Enum.each(meta[:files], &Mix.shell.info("    #{&1}"))
  end

  defp revert(meta, version, auth) do
    case Hex.API.delete_release(meta[:app], version, auth) do
      { 204, _ } ->
        Mix.shell.info("Reverted #{meta[:app]} v#{meta[:version]}")
      { code, body } ->
        Mix.shell.error("Reverting #{meta[:app]} v#{meta[:version]} failed! (#{code})")
        Hex.Util.print_error_result(code, body)
        false
    end
  end

  defp create_package?(meta, auth) do
    case Hex.API.new_package(meta[:app], meta, auth) do
      { code, _ } when code in [200, 201] ->
        true
      { code, body } ->
        Mix.shell.error("Updating package #{meta[:app]} failed (#{code})")
        Hex.Util.print_error_result(code, body)
        false
    end
  end

  defp create_release(meta, auth) do
    tarball = Hex.Tar.create(meta, meta[:files])

    case Hex.API.new_release(meta[:app], tarball, auth) do
      { code, _ } when code in [200, 201] ->
        Mix.shell.info("Published #{meta[:app]} v#{meta[:version]}")
      { code, body } ->
        Mix.shell.error("Pushing #{meta[:app]} v#{meta[:version]} failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp dependencies(meta) do
    deps = Enum.map(meta[:deps] || [], &Hex.Mix.dep/1)
    { include, exclude } = Enum.partition(deps, &(package_dep?(&1) and prod_dep?(&1)))
    include = Enum.map(include, fn { app, req, _opts } -> { app, req } end)
    exclude = Enum.map(exclude, fn { app, _req, _opts } -> app end)
    { include, exclude }
  end

  defp prod_dep?({ _app, _req, opts }) do
    if only = opts[:only], do: :prod in List.wrap(only), else: true
  end

  defp package_dep?({ app, _req, opts }) do
    Enum.find(Mix.SCM.available, fn scm ->
      scm.accepts_options(app, opts)
    end) == Hex.SCM
  end

  defp expand_paths(paths) do
    Enum.flat_map(paths, fn path ->
      if File.dir?(path) do
        Path.wildcard(Path.join(path, "**"))
      else
        Path.wildcard(path)
      end
    end) |> Enum.uniq
  end
end
