defmodule Mix.Tasks.Hex.Fetch do
  use Mix.Task

  @shortdoc "Fetches a Hex package"

  @moduledoc """
  Fetches a Hex package.

      mix hex.fetch PACKAGE [VERSION]

  Fetches and optionally unpacks the Hex package. The latest stable version of
  the package will be fetched unless a specific version is given.

  ## Command line options

    * `--unpack` - unpacks the package tarball.
    * `-o`, `--output` - sets output directory. Default: current directory
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
    dest =
      if opts[:output] do
        Path.expand(opts[:output])
      else
        File.cwd!()
      end

    tar_file = Path.join(dest, "#{name}-#{version}.tar")
    opts = opts |> Hex.organization_to_repo() |> Keyword.put_new(:repo, "hexpm")
    repo = opts[:repo]

    if File.exists?(tar_file) do
      Hex.Shell.info("Package already fetched: #{tar_file}")
    else
      request_package_from_mirror(tar_file, name, version, repo)

      if opts[:unpack] do
        Hex.unpack_and_verify_tar!(tar_file, dest, repo, name, version)
        File.rm_rf!(tar_file)
      end

      Hex.Shell.info("Package fetched at: #{dest}")
    end
  end

  defp request_package_from_mirror(tar_file, package, version, repo) do
    case Hex.Repo.get_tarball(repo, package, version, nil) do
      {:ok, {200, body, _}} ->
        tar_file |> Path.dirname() |> File.mkdir_p!()
        File.write!(tar_file, body)

      _ ->
        Mix.raise("No package with name #{package} or version #{version}")
    end
  end
end
