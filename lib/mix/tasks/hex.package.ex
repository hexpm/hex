defmodule Mix.Tasks.Hex.Package do
  use Mix.Task

  @default_repo "hexpm"

  @shortdoc "Allows audit of Hex packages"

  @moduledoc """
  Allows fetching (and unpacking) of package tarballs for easier auditing of
  packages.

  ## Fetch package

  Downloads a package tarball to the current directory.

  mix hex.package fetch PACKAGE VERSION [--unpack]

  ## Command line options

  * `--unpack` - Unpacks the tarball after downloading it
  """

  @switches [unpack: :boolean]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)
    unpack = Keyword.get(opts, :unpack, false)

    case args do
      ["fetch", package, version] ->
        case Hex.Repo.get_tarball(@default_repo, package, version, nil) do
          {:ok, {200, tar_body, _headers}} ->
            abs_path = Path.absname("#{package}-#{version}")
            tar_path = "#{abs_path}.tar"
            File.write!(tar_path, tar_body)
            message =
              if unpack do
                unpack_tarball!(tar_path, abs_path)
                "#{package} v#{version} extracted to #{abs_path}"
              else
                "#{package} v#{version} downloaded in #{tar_path}"
              end
            Hex.Shell.info(message)

          {:ok, {code, _body, _headers}} ->
            Hex.Shell.error("Request failed (#{code})")

          {:error, :timeout} ->
            reason = """
              Request failed (:timeout),
              If this happens consistently, adjust your concurrency and timeout settings:
              HEX_HTTP_CONCURRENCY=1 HEX_HTTP_TIMEOUT=120 mix hex.package fetch #{package} #{version}
            """
            Hex.Shell.error(reason)

          {:error, reason} ->
            Hex.Shell.error("Request failed (#{inspect(reason)})")
        end

          _ ->
            Mix.raise("""
              Invalid arguments, expected one of:

              mix hex.package fetch PACKAGE VERSION [--unpack]
            """)
    end
  end

  defp unpack_tarball!(tar_path, dest_path) do
    Hex.unpack_tar!(tar_path, dest_path)
    File.rm!(tar_path)
  end
end
