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

  IMPORTANT! Make sure to create the commit for this release before running
  task. Also, please make sure that the package compiles correctly and that all
  tests pass.

  ## Command line options

  * `--user`, `-u` - Username of package owner (required)

  * `--password`, `-p` - Password of package owner (required)

  * `--tag`, `-t` - Git tag of release (required)

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

  As can be seen git dependencies works alongside git dependencies. Important
  to note is that non-packaged dependencies will not be used during hex's
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

  @aliases [u: :user, p: :password, t: :tag]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :password, :tag])

    Mix.Task.run "compile"
    Mix.Project.get!
    config = Mix.project

    Hex.start_api
    if create_package?(config, opts) do
      create_release(config, opts)
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

  defp create_release(config, opts) do
    # TODO: Filter on :only option
    reqs    = Hex.Mix.deps_to_requests(config[:deps] || [])
    auth    = Keyword.take(opts, [:user, :password])
    git_url = git_remote(config[:package])
    git_ref = opts[:tag]

    case Hex.API.new_release(config[:app], config[:version], git_url, git_ref, reqs, auth) do
      { code, _ } when code in [200, 201] ->
        Mix.shell.info("Updating package #{config[:app]} and creating " <>
          "release #{config[:version]} was successful!")
      { code, body } ->
        Mix.shell.error("Creating release #{config[:app]} #{config[:version]} failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  def git_remote(opts) do
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
