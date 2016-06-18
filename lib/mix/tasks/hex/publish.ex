defmodule Mix.Tasks.Hex.Publish do
  use Mix.Task
  alias Mix.Hex.Utils
  alias Mix.Hex.Build

  @shortdoc "Publishes a new package version"

  @moduledoc """
  Publishes a new version of your package and update the package.

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

    * `:maintainers` - List of names and/or emails of maintainers.

    * `:licenses` - List of licenses used by the package.

    * `:links` - Map of links relevant to the package.

    * `:build_tools` - List of build tools that can build the package. Hex will
      try to automatically detect the build tools, it will do this based on the
      files in the package. If a "rebar" or "rebar.config" file is present Hex
      will mark it as able to build with rebar. This detection can be overridden
      by setting this field.
  """

  @switches [revert: :string, progress: :boolean]

  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    build        = Build.prepare_package!
    meta         = build[:meta]
    package      = build[:package]
    exclude_deps = build[:exclude_deps]
    auth         = Utils.auth_info()

    if version = opts[:revert] do
      revert(meta, version, auth)
    else
      Hex.Shell.info("Publishing #{meta[:name]} #{meta[:version]}")
      Build.print_info(meta, exclude_deps, package[:files])

      print_link_to_coc()

      if Hex.Shell.yes?("Proceed?") do
        progress? = Keyword.get(opts, :progress, true)
        create_release(meta, auth, progress?)
      end
    end
  end

  defp print_link_to_coc() do
    Hex.Shell.info "Before publishing, please read Hex Code of Conduct: https://hex.pm/policies/codeofconduct"
  end

  defp revert(meta, version, auth) do
    version = Utils.clean_version(version)

    case Hex.API.Release.delete(meta[:name], version, auth) do
      {code, _, _} when code in 200..299 ->
        Hex.Shell.info("Reverted #{meta[:name]} #{version}")
      {code, body, _} ->
        Hex.Shell.error("Reverting #{meta[:name]} #{version} failed")
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp create_release(meta, auth, progress?) do
    {tarball, checksum} = Hex.Tar.create(meta, meta[:files])

    progress =
      if progress? do
        Utils.progress(byte_size(tarball))
      else
        Utils.progress(nil)
      end

    case Hex.API.Release.new(meta[:name], tarball, auth, progress) do
      {code, _, _} when code in 200..299 ->
        Hex.Shell.info("\nPublished at #{Hex.Utils.hex_package_url(meta[:name], meta[:version])} (#{checksum})")
        Hex.Shell.info("Don't forget to upload your documentation with `mix hex.docs`")
      {code, body, _} ->
        Hex.Shell.error("\nPushing #{meta[:name]} #{meta[:version]} failed")
        Hex.Utils.print_error_result(code, body)
    end
  end
end
