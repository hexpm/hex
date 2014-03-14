defmodule Mix.Tasks.Hex.Release do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Create a new package release"

  @moduledoc """
  Create a new package release and update package meta data.

  `mix hex.release -u username -p password -t 0.13.0`

  If it is a new package being released it will be created and the user
  specified in `username` will be the package owner. Only package owners can
  create new releases.

  A release can be amended or reverted with `--revert` up to one hour after its
  creation. If you want to revert a release that is more than one hour old you
  need to contact an administrator.

  ## Command line options

  * `--user`, `-u` - Username of package owner (required)

  * `--pass`, `-p` - Password of package owner (required)

  * `--revert version` - Revert given release

  ## Configuration

  * `:app` - Package name (required)

  * `:version` - Release version (required)

  * `:deps` - List of package dependencies (see Dependencies below)

  * `:package` - Hex specific configuration (see Package configuration below) (required)

  ## Dependencies

  Dependencies are defined in mix's dependency format. But instead of using
  `:git` or `:path` as the SCM `:package` is used.

      defp deps do
        [ { :ecto, "~> 0.1.0", package: true },
          { :postgrex, "~> 0.3.0", package: true },
          { :cowboy, github: "extend/cowboy" } ]
      end

  As can be seen package dependencies works alongside git dependencies.
  Important to note is that non-packaged dependencies will not be used during
  dependency resolution and neither will be they listed as dependencies under
  a release.

  ## Package configuration

  Additional metadata of the package can optionally be defined, but it is very
  recommended to do so.

  * `:files` - List of files and directories to include in the release (required)

  * `:description` - Description of the project in a few paragraphs

  * `:contributors` - List of names of contributors

  * `:licenses` - List of licenses used by the package

  * `:links` - Dictionary of links
  """

  @switches [revert: :string]
  @aliases [u: :user, p: :pass]

  @default_files ["lib", "priv", "mix.exs", "README*", "readme*"]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, switches: @switches, aliases: @aliases)
    user_config = Hex.Mix.read_config
    opts = Util.config_opts(opts, user_config)
    Util.required_opts(opts, [:user, :pass])
    Hex.start_api

    if version = opts[:revert] do
      revert(Mix.project, version, opts)
    else
      Mix.Task.run "compile"
      Mix.Project.get!
      config = Mix.project
      reqs   = get_requests(config)
      files  = expand_paths(config[:package][:files] || @default_files)
      meta   = Keyword.take(config, [:app, :version])
      meta   = meta ++ [requirements: reqs, files: files] ++ (config[:package] || [])
      auth   = Keyword.take(opts, [:user, :pass])

      print_info(meta)

      if create_package?(meta, auth) do
        create_release(meta, auth)
      end
    end
  end

  defp print_info(meta) do
    # TODO: Run dependency resolution (don't use mix.lock), check that deps
    # are valid packages and has valid version requirements

    Mix.shell.info("Pushing release #{meta[:app]} v#{meta[:version]}")
    Mix.shell.info("  Dependencies:")
    Mix.shell.info("  Files: !TODO!")

    Enum.each(meta[:requirements], fn { app, req } ->
      Mix.shell.info("    #{app}: #{req}")
    end)

    Mix.shell.yes?("Proceed?")
  end

  defp revert(meta, version, auth) do
    case Hex.API.delete_release(meta[:app], version, auth) do
      { 204, _ } ->
        Mix.shell.info("Successfully reverted #{meta[:app]} v#{meta[:version]}")
      { code, body } ->
        Mix.shell.error("Reverting #{meta[:app]} v#{meta[:version]} failed! (#{code})")
        Util.print_error_result(code, body)
        false
    end
  end

  defp create_package?(meta, auth) do
    case Hex.API.new_package(meta[:app], meta, auth) do
      { code, _ } when code in [200, 201] ->
        true
      { code, body } ->
        Mix.shell.error("Updating package #{meta[:app]} failed! (#{code})")
        Util.print_error_result(code, body)
        false
    end
  end

  defp create_release(meta, auth) do
    tarball = Hex.Tar.create(meta, meta[:files])

    case Hex.API.new_release(meta[:app], tarball, auth) do
      { code, _ } when code in [200, 201] ->
        Mix.shell.info("Successfully pushed #{meta[:app]} v#{meta[:version]}!")
      { code, body } ->
        Mix.shell.error("Pushing #{meta[:app]} v#{meta[:version]} failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  defp get_requests(meta) do
    Enum.filter(meta[:deps] || [], &prod_dep?(&1))
    |> Hex.Mix.deps_to_requests
  end

  defp prod_dep?({ _app, opts }) do
    if only = opts[:only], do: :prod in List.wrap(only), else: true
  end

  defp prod_dep?({ _app, _req, opts }) do
    if only = opts[:only], do: :prod in List.wrap(only), else: true
  end

  defp expand_paths(paths) do
    Enum.flat_map(paths, fn path ->
      cond do
        not File.exists?(path) ->
          []
        File.dir?(path) ->
          Path.wildcard(Path.join(path, "**"))
        true ->
          Path.wildcard(path)
      end
    end) |> Enum.uniq
  end
end
