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

  * `--tag`, `-t` - Git tag of release (required)

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

  * `:github` or `:git` - Git remote used when fetching package (required)

  * `:description` - Description of the project in a few paragraphs

  * `:contributors` - List of names of contributors

  * `:licenses` - List of licenses used by the package

  * `:links` - Dictionary of links
  """

  @switches [revert: :string]
  @aliases [u: :user, p: :pass, t: :tag]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, switches: @switches, aliases: @aliases)
    user_config = Hex.Mix.read_config
    opts = Util.config_opts(opts, user_config)
    Util.required_opts(opts, [:user, :pass])
    Hex.start_api

    if version = opts[:revert] do
      revert(Mix.project, version, opts)
    else
      Util.required_opts(opts, [:tag])
      Mix.Task.run "compile"
      Mix.Project.get!
      config = Mix.project
      reqs   = get_requests(config)

      print_info(config, reqs, opts)

      if create_package?(config, opts) do
        create_release(config, reqs, opts)
      end
    end
  end

  defp print_info(config, reqs, opts) do
    # TODO: Fail if a dependency is not a package

    Mix.shell.info("Pushing release #{config[:app]} #{config[:version]}")
    Mix.shell.info("  Tag: #{opts[:tag]}")
    Mix.shell.info("  Dependencies:")

    Enum.each(reqs, fn { app, req, _override? } ->
      Mix.shell.info("    #{app}: #{req}")
    end)

    Mix.shell.yes?("Proceed?")
  end

  defp revert(config, version, opts) do
    case Hex.API.delete_release(config[:app], version, opts) do
      { 204, _ } ->
        Mix.shell.info("Successfully reverted release #{config[:app]} #{config[:version]}")
      { code, body } ->
        Mix.shell.error("Reverting release #{config[:app]} #{config[:version]} failed! (#{code})")
        Util.print_error_result(code, body)
        false
    end
  end

  defp create_package?(config, opts) do
    meta = config[:package] || []

    case Hex.API.new_package(config[:app], meta, opts) do
      { code, _ } when code in [200, 201] ->
        true
      { code, body } ->
        Mix.shell.error("Updating package #{config[:app]} failed! (#{code})")
        Util.print_error_result(code, body)
        false
    end
  end

  defp create_release(config, reqs, opts) do
    deps    = Enum.map(reqs, fn { app, req, _override? } -> { app, req } end)
    auth    = Keyword.take(opts, [:user, :pass])
    git_url = git_remote(config[:package])
    git_ref = opts[:tag]

    case Hex.API.new_release(config[:app], config[:version], git_url, git_ref, deps, auth) do
      { code, _ } when code in [200, 201] ->
        Mix.shell.info("Successfully updated packaged and pushed new release!")
      { code, body } ->
        Mix.shell.error("Creating release #{config[:app]} #{config[:version]} failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  defp get_requests(config) do
    Enum.filter(config[:deps] || [], &prod_dep?(&1))
    |> Hex.Mix.deps_to_requests
  end

  defp prod_dep?({ _app, opts }) do
    if only = opts[:only], do: :prod in List.wrap(only), else: true
  end

  defp prod_dep?({ _app, _req, opts }) do
    if only = opts[:only], do: :prod in List.wrap(only), else: true
  end

  defp git_remote(opts) do
    cond do
      gh = opts[:github] ->
        "git://github.com/#{gh}.git"
      git = opts[:git] ->
        git
      true ->
        raise Mix.Error, message: "Missing git remote configuration in mix.exs"
    end
  end
end
