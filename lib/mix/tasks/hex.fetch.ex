defmodule Mix.Tasks.Hex.Fetch do
  use Mix.Task

  @shortdoc "Fetches a Hex package"

  @moduledoc """
  Fetches a Hex package.

      mix hex.fetch PACKAGE [VERSION]

  Fetches and optionally unpacks the Hex package. The latest stable version of
  the package will be fetched unless a specific version is given.

  ## Command line options

    * `--unpack` - Unpacks the fetched package tarball into a directory.
      See `--output` below for setting the output path.

    * `-o`, `--output` - Sets output path. When used with `--unpack` it means
      the directory (Default: `<app>-<version>`). Otherwise, it specifies
      tarball path (Default: `<app>-<version>.tar`)

    * `--organization ORGANIZATION` - the organization the package belongs to

  """

  @switches [output: :string, unpack: :boolean, organization: :string]
  @aliases [o: :output]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    case args do
      [package] ->
        fetch_package([package], opts)

      [package, version] ->
        fetch_package([package, version], opts)

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.fetch PACKAGE [VERSION] [--output PATH] [--unpack]
        """)
    end
  end

  defp fetch_package([name], opts) do
    latest_version = Mix.Tasks.Hex.find_package_latest_version(opts[:organization], name)
    fetch_package([name, latest_version], opts)
  end

  defp fetch_package([name, version], opts) do
    output =
      if opts[:unpack] do
        Keyword.get(opts, :output, "#{name}-#{version}")
      else
        Keyword.get(opts, :output, "#{name}-#{version}.tar")
      end

    opts = opts |> Hex.organization_to_repo() |> Keyword.put_new(:repo, "hexpm")
    repo = opts[:repo]

    if File.exists?(output) do
      Hex.Shell.info("Package already fetched: #{output}")
    else
      tarball = get_tarball!(name, version, repo)
      Hex.Registry.Server.open(check_version: false)
      Hex.Registry.Server.prefetch([{repo, name}])
      registry_checksum = Hex.Registry.Server.checksum(repo, name, version)

      if opts[:unpack] do
        Hex.unpack_and_verify_tar!({:binary, tarball}, output, registry_checksum)
        Hex.Shell.info("Package fetched and unpacked to: #{output}")
      else
        output |> Path.dirname() |> File.mkdir_p!()
        File.write!(output, tarball)
        Hex.unpack_and_verify_tar!({:binary, tarball}, :memory, registry_checksum)
        Hex.Shell.info("Package fetched at: #{output}")
      end
    end
  end

  defp get_tarball!(package, version, repo) do
    case Hex.Repo.get_tarball(repo, package, version, nil) do
      {:ok, {200, body, _}} ->
        body

      _ ->
        Mix.raise("No package with name #{package} or version #{version}")
    end
  end
end
