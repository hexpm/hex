defmodule Hex.Package do
  @moduledoc false

  def default_files() do
    ~w(lib priv .formatter.exs mix.exs README* readme* LICENSE* license*
      CHANGELOG* changelog* src c_src Makefile*)
  end

  def configuration_doc() do
    """
    ## Configuration

    * `:app` - Package name (required).
    * `:version` - Package version (required).
    * `:deps` - List of package dependencies (see [Dependencies](#module-dependencies) below).
    * `:description` - Short description of the project.
    * `:package` - Hex specific configuration (see [Package configuration](#module-package-configuration) below).

    ## Dependencies

    Dependencies are defined in mix's dependency format. But instead of using
    `:git` or `:path` as the SCM `:package` is used.

        defp deps() do
          [
            {:ecto, "~> 0.1.0"},
            {:postgrex, "~> 0.3.0"},
            {:cowboy, github: "extend/cowboy"}
          ]
        end

    As can be seen Hex package dependencies works alongside git dependencies.
    Important to note is that non-Hex dependencies will not be used during
    dependency resolution and neither will they be listed as dependencies of the
    package.

    ## Package configuration

    Additional metadata of the package can optionally be defined, but it is very recommended to do so.

      * `:name` - Set this if the package name is not the same as the application
         name.
      * `:files` - List of files and directories to include in the package,
        can include wildcards. Defaults to `#{inspect(default_files())}`.
        Files created by operating systems, editors and version control (such
        as `.DS_Store`, `*~` and `.git`) are excluded unless they are listed
        without wildcards. The build and dependency directories, and the
        package tarball and unpack directory are always excluded.
      * `:exclude_patterns` - List of patterns matching files and directories to
        exclude from the package.
      * `:executables` - List of files and directories in the package whose
        files are executable, can include wildcards. By default a file is
        executable in the package if the owner execute permission is set on
        disk. When this option is set the permissions on disk are not used, use
        it when the file system doesn't store execute permissions, such as on
        Windows.
      * `:licenses` - List of licenses used by the package. Use SPDX license
        identifiers or `LicenseRef-<idstring>` for custom licenses included in
        the package.
      * `:links` - Map of links relevant to the package.
      * `:build_tools` - List of build tools that can build the package. Hex will
        try to automatically detect the build tools based on the files in the
        package. If a `rebar` or `rebar.config` file is present Hex will mark it
        as able to build with rebar. This detection can be overridden by setting
        this field.
      * `:secret_scan` - Configuration for the repository's secret scanning of
        the package. Supports `:ignore`, a list of file path globs whose
        findings are suppressed, for paths that deliberately ship secret-like
        values such as test fixtures:

            secret_scan: [ignore: ["test/fixtures/**"]]
    """
  end
end
