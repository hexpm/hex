defmodule Mix.Tasks.Hex.Release do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Create a new package release"

  @moduledoc """
  Create a new package release and update package meta data.

  `mix hex.release -u username -p password`

  If it is a new package being released it will be created and the user
  specified in `username` will be the package owner. Only package owners can
  create new releases.

  IMPORTANT! Make sure to create the commit for this release before running
  task. Also, please make sure that the package compiles correctly and that all
  tests pass.

  ## Command line options

  * `--user`, `-u` - Username of package owner (required)

  * `--password`, `-p` - Password of package owner (required)

  ## Configuration

  * `:app` - Package name (required)

  * `:version` - Release version (required)

  * `:deps` - List of package dependencies (see Dependencies below)

  * `:package` - List of package metadata (see Metadata below)

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

  ## Metadata

  Additional metadata of the package can optionally be defined, but it is very
  recommended to do so.

  * `:description` - Description of the project in a few paragraphs

  * `:contributors` - List of names of contributors

  * `:licenses` - List of licenses used by the package

  * `:links` - Dictionary of links
  """

  @aliases [u: :user, p: :password]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :password])

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
    reqs = Hex.Mix.deps_to_requirements(config[:deps] || [])
    git_url = config[:source_url]
    git_ref = git_ref()

    unless git_url do
      raise Mix.Error, message: "Missing source_url, specifying the git repo url, option in mix.exs"
    end

    case Hex.API.new_release(config[:app], config[:version], git_url, git_ref, reqs, opts) do
      { 201, _ } ->
        Mix.shell.info("Updating package #{config[:app]} and creating " <>
          "release #{config[:version]} was successful!")
      { code, body } ->
        Mix.shell.error("Creating release #{config[:app]} #{config[:version]} failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end

  @ref_regex ~r/^
      [0-9A-Fa-f]+
      $/x

  defp git_ref do
    ref = System.cmd("git rev-parse --verify --quiet HEAD")
          |> String.strip

    unless Regex.match?(@ref_regex, ref) do
      raise Mix.Error, message: "Invalid git ref, are you in a git repository?"
    end

    ref
  end
end
