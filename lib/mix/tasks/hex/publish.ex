defmodule Mix.Tasks.Hex.Publish do
  use Mix.Task
  alias Mix.Hex.Utils
  alias Mix.Hex.Build

  @shortdoc "Publishes a new package version"

  @moduledoc """
  Publishes a new version of your package and update the package.

  `mix hex.publish package`

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
    build = Build.prepare_package!

    {opts, rest, _} = OptionParser.parse(args, switches: @switches)
    auth         = Utils.auth_info()

    if version = opts[:revert] do
      revert(build, version, auth)
    else
      case rest do
        ["package"] ->
          create_package(build, auth, opts)
        ["docs"] ->
          create_docs(auth, opts, args)
        _otherwise ->
          message = """
            Invalid arguments, expected one of:
              mix hex.publish package
              mix hex.publish docs
            """
          Mix.raise message
      end
    end
  end

  defp create_package(build, auth, opts) do
    meta = build[:meta]
    package = build[:package]
    exclude_deps = build[:exclude_deps]

    Hex.Shell.info("Publishing #{meta[:name]} v#{meta[:version]}")
    Build.print_info(meta, exclude_deps, package[:files])

    print_link_to_coc()

    if Hex.Shell.yes?("Proceed?") do
      progress? = Keyword.get(opts, :progress, true)
      create_release(meta, auth, progress?)
    end
  end

  defp create_docs(auth, opts, args) do
    Mix.Project.get!

    config  = Mix.Project.config
    app     = config[:app]
    version = config[:version]

    Mix.Task.run("docs", args)

    directory = docs_dir()

    unless File.exists?("#{directory}/index.html") do
      Mix.raise "File not found: #{directory}/index.html"
    end

    progress? = Keyword.get(opts, :progress, true)
    tarball = build_tarball(app, version, directory)
    send_tarball(app, version, tarball, auth, progress?)
  end

  defp print_link_to_coc() do
    Hex.Shell.info "Before publishing, please read Hex Code of Conduct: https://hex.pm/docs/codeofconduct"
  end

  defp revert(build, version, auth) do
    version = Utils.clean_version(version)
    meta = build[:meta]

    case Hex.API.Release.delete(meta[:name], version, auth) do
      {code, _} when code in 200..299 ->
        Hex.Shell.info("Reverted #{meta[:name]} v#{version}")
        Hex.Shell.info("Reverted docs for #{meta[:name]} v#{version}")
      {code, body} ->
        Hex.Shell.error("Reverting #{meta[:name]} v#{version} failed")
        Hex.Shell.error("Reverting docs for #{meta[:name]} v#{version} failed")
        Hex.Utils.print_error_result(code, body)
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
        # TODO: Only print this URL if we use the default API URL
        Hex.Shell.info("\nPublished at #{Hex.Utils.hex_package_url(meta[:name], meta[:version])}")
        Hex.Shell.info("Don't forget to upload your documentation with `mix hex.docs`")
      {code, body} ->
        Hex.Shell.error("\nPushing #{meta[:name]} v#{meta[:version]} failed")
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp build_tarball(app, version, directory) do
    tarball = "#{app}-#{version}-docs.tar.gz"
    files = files(directory)
    :ok = :erl_tar.create(tarball, files, [:compressed])
    data = File.read!(tarball)

    File.rm!(tarball)
    data
  end

  defp send_tarball(app, version, tarball, auth, progress?) do
    if progress? do
      progress = Utils.progress(byte_size(tarball))
    else
      progress = Utils.progress(nil)
    end

    case Hex.API.ReleaseDocs.new(app, version, tarball, auth, progress) do
      {code, _} when code in 200..299 ->
        Hex.Shell.info ""
        Hex.Shell.info "Published docs for #{app} v#{version}"
        # TODO: Only print this URL if we use the default API URL
        Hex.Shell.info "Hosted at #{Hex.Utils.hexdocs_url(app, version)}"
      {code, _} when code == 404 ->
        Hex.Shell.info ""
        Hex.Shell.error "Pushing docs for #{app} v#{version} is not possible due to the package not be published"
      {code, body} ->
        Hex.Shell.info ""
        Hex.Shell.error "Pushing docs for #{app} v#{version} failed"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp files(directory) do
    "#{directory}/**"
    |> Path.wildcard
    |> Enum.filter(&File.regular?/1)
    |> Enum.map(&{relative_path(&1, directory), File.read!(&1)})
  end

  defp relative_path(file, dir) do
    Path.relative_to(file, dir)
    |> String.to_char_list
  end

  defp docs_dir do
    cond do
      File.exists?("doc") ->
        "doc"
      File.exists?("docs") ->
        "docs"
      true ->
        Mix.raise("Documentation could not be found. Please ensure documentation is in the doc/ or docs/ directory.")
    end
  end
end
