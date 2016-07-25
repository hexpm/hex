defmodule Mix.Tasks.Hex.Build do
  use Mix.Task
  alias Mix.Hex.Build

  @shortdoc "Builds a new package version locally"

  @moduledoc """
  Builds a new local version of your package.

  The package .tar file is created in the current directory, but is not pushed
  to the repository. An app named `foo` at version `1.2.3` will be built as
  `foo-1.2.3.tar`.

      mix hex.build

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

  Additional package metadata is optional, but highly recommended.

    * `:name` - Set this if the package name is not the same as the application
       name.

    * `:files` - List of files and directories to include in the package,
      can include wildcards. Defaults to `["lib", "priv", "mix.exs", "README*",
      "readme*", "LICENSE*", "license*", "CHANGELOG*", "changelog*", "src"]`.

    * `:maintainers` - List of names and/or emails of maintainers.

    * `:licenses` - List of licenses used by the package.

    * `:links` - Map of links relevant to the package.

    * `:build_tools` - List of build tools that can build the package. Hex will
      try to automatically detect the build tools based on the files in the
      package. If a "rebar" or "rebar.config" file is present Hex will mark it
      as able to build with rebar. This detection can be overridden by setting
      this field.
  """

  def run(_args) do
    Hex.start
    build = Build.prepare_package

    meta = build.meta
    package = build.package
    exclude_deps = build.exclude_deps

    Hex.Shell.info("Building #{meta.name} #{meta.version}")
    Build.print_info(meta, exclude_deps, package[:files])

    {_tar, checksum} = Hex.Tar.create(meta, meta.files, false)
    Hex.Shell.info("Package checksum: #{checksum}")
  end
end
