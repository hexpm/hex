defmodule Mix.Tasks.Hex.PublicKeys do
  use Mix.Task

  @shortdoc  "Manages Hex public keys"

  @moduledoc """
  Public keys are used by Hex to verify the registry file.

  Hex by default ships with a public key for Hex.pm but new
  ones can be added for other repositories.

  To list all available keys:

      mix hex.public_keys list

  To list all available keys showing the keys themselves:

      mix hex.public_keys list --detailed

  To add a new key:

      mix hex.public_keys add URL_TO_REPO LOCAL_PATH_TO_KEY

  To remove a key:

      mix hex.public_keys remove URL_TO_REPO

  Be careful when adding new keys. Only add keys from sources you
  trust.

  Public keys are by default stored in your HEX_HOME under the
  public_keys directory.

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`
  """

  @switches [force: :boolean, detailed: :boolean]

  def run(args) do
    Hex.start
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    case args do
      ["list"] ->
        list(opts)
      ["add", id, path|_] ->
        add(id, path, opts)
      ["remove", key|_] ->
        remove(key, opts)
    _ ->
      Mix.raise """
      Invalid arguments, expected one of:
      mix hex.public_keys list
      mix hex.public_keys add URL_TO_REPO LOCAL_PATH_TO_KEY
      mix hex.public_keys remove URL_TO_REPO
      """
    end
  end

  defp list(opts) do
    for {id, key} <- Hex.Crypto.PublicKey.public_keys do
      Hex.Shell.info "* #{id}"
      if opts[:detailed] do
        Hex.Shell.info "\n#{key}"
      end
    end

    Hex.Shell.info "Public keys (except in-memory ones) installed at: " <>
                   Hex.Crypto.PublicKey.public_keys_path()
  end

  defp add(id, source, opts) do
    data = File.read!(source)
    file = Base.url_encode64(id)
    dest = Path.join(Hex.Crypto.PublicKey.public_keys_path, file)

    # Validate the key is good
    _ = Hex.Crypto.PublicKey.decode!(id, data)

    if opts[:force] || should_install?(id, dest) do
      File.mkdir_p!(Hex.Crypto.PublicKey.public_keys_path)
      File.write!(dest, data)
    end
  end

  defp remove(id, _opts) do
    file = Base.url_encode64(id)
    path = Path.join(Hex.Crypto.PublicKey.public_keys_path, file)

    if File.exists?(path) do
      File.rm!(path)
      Hex.Shell.info "Removed key for #{id}"
    else
      Mix.raise "No installed key for #{id}"
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
