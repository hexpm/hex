defmodule Mix.Tasks.Hex.PublicKeys do
  use Mix.Task

  @shortdoc  "Manages Hex public keys"

  @moduledoc """
  Public keys are used by Hex to verify the registry file.

  Hex by default ships with a public key for Hex.pm but new
  ones can be added for other repositories.

  To list all available keys:

      $ mix hex.public_keys

  To list all available keys showing the keys themselves:

      $ mix hex.public_keys --detailed

  To add a new key:

      $ mix hex.public_keys URL_TO_REPO LOCAL_PATH_TO_KEY

  Be careful when adding new keys. Only add keys from sources you
  trust.

  Public keys are by default stored in your HEX_HOME under the
  public_keys directory.

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`
  """
  @spec run(OptionParser.argv) :: true
  def run(argv) do
    Hex.start

    {opts, argv, _} = OptionParser.parse(argv, switches: [force: :boolean, detailed: :boolean])

    case argv do
      [] ->
        show(opts)
      [key, path|_] ->
        install(key, path, opts)
    _ ->
      Mix.raise "Invalid arguments, expected one of:\nmix hex.public_keys\nmix hex.public_keys URL_TO_REPO LOCAL_PATH_TO_KEY"
    end
  end

  defp show(opts) do
    for {id, key} <- Hex.PublicKey.public_keys do
      Hex.Shell.info "* #{id}"
      if opts[:detailed] do
        Hex.Shell.info "\n#{key}"
      end
    end

    Hex.Shell.info "Public keys (except in-memory ones) installed at: #{Hex.PublicKey.public_keys_path()}"
  end

  defp install(key, source, opts) do
    data = File.read!(source)
    file = Base.url_encode64(key)
    dest = Path.join(Hex.PublicKey.public_keys_path, file)

    # Validate the key is good
    _ = Hex.PublicKey.decode!(key, data)

    if opts[:force] || should_install?(key, dest) do
      File.mkdir_p!(Hex.PublicKey.public_keys_path)
      File.write!(dest, data)
    end
  end

  defp should_install?(id, dest) do
    if File.exists?(dest) do
      Hex.Shell.yes?("There is already a public key for #{id}.\n" <>
                     "Are you sure you want to replace it?")
    else
      Hex.Shell.yes?("Are you sure you want to install public key #{id}?")
    end
  end
end
