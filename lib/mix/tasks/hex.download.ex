defmodule Mix.Tasks.Hex.Download do
  use Mix.Task

  @shortdoc "Fetches a Hex package"

  @moduledoc """
  Fetches a Hex package.

      mix hex.download PACKAGE [VERSION]

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

        mix hex.download PACKAGE [VERSION] [--output PATH] [--unpack]
        """)
    end
  end

  defp fetch_package([name], opts) do
    latest_version = Mix.Tasks.Hex.find_package_latest_version(opts[:organization], name)
    fetch_package([name, latest_version], opts)
  end

  defp fetch_package([name, version], opts) do
    parent_directory =
      if opts[:output] do
        Path.expand(opts[:output])
      else
        File.cwd!()
      end

    target = Path.join(parent_directory, "#{name}-#{version}.tar")
    organization = opts[:organization]

    if File.exists?(target) do
      verify_checksum(target, name, version, organization)
      Hex.Shell.info("Package already fetched: #{target}")
    else
      request_package_from_mirror(target, name, version, organization)

      if opts[:unpack] do
        Hex.Tar.unpack(target, parent_directory)
        File.rm_rf!(target)
      end

      Hex.Shell.info("Package fetched at: #{parent_directory}")
    end
  end

  defp request_package_from_mirror(target, package, version, organization) do
    case Hex.Repo.get_tarball(organization, package, version, nil) do
      {:ok, {200, body, _}} ->
        target |> Path.dirname() |> File.mkdir_p!()
        File.write!(target, body)

      _ ->
        Mix.raise("No package with name #{package} or version #{version}")
    end
  end

  defp verify_checksum(tarball, package, version, organization) do
    case :hex_erl_tar.extract(tarball, [:memory]) do
      {:ok, files} ->
        files = Enum.into(files, %{})

        registry_checksum =
          Hex.Registry.Server.checksum(organization, package, version)
          |> Base.encode16(case: :lower)

        {:ok, tarball_checksum} = Base.decode16(files["CHECKSUM"], case: :lower)

        if registry_checksum != tarball_checksum do
          Mix.raise("Checksum mismatch in tarball against registry")
        end

      :ok ->
        Mix.raise("Unpacking tarball failed: tarball empty")

      {:error, reason} ->
        error_message = reason |> :hex_erl_tar.format_error() |> List.to_string()
        Mix.raise("Unpacking tarball failed: " <> error_message)
    end
  end
end
