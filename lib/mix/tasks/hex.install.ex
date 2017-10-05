defmodule Mix.Tasks.Hex.Install do
  use Mix.Task

  @hex_list_path    "/installs/hex-1.x.csv"
  @hex_archive_path "/installs/[ELIXIR_VERSION]/hex-[HEX_VERSION].ez"
  @public_keys_html "https://repo.hex.pm/installs/public_keys.html"

  @shortdoc false

  @moduledoc """
  Manually installs specific Hex version.

      mix hex.install VERSION
  """

  def run(args) do
    Hex.start()
    {_, args} = Hex.OptionParser.parse!(args)

    case args do
      [version] ->
        install(version)
      _ ->
        Mix.raise """
        Invalid arguments, expected:

        mix hex.install VERSION
        """
    end
  end

  defp install(hex_version) do
    hex_url = Hex.Repo.get_repo("hexpm").url
    csv_url = hex_url <> @hex_list_path

    case find_matching_versions_from_signed_csv!("Hex", csv_url, hex_version) do
      {elixir_version, sha512} ->
        archive_url =
          (hex_url <> @hex_archive_path)
          |> String.replace("[ELIXIR_VERSION]", elixir_version)
          |> String.replace("[HEX_VERSION]", hex_version)

        Mix.Tasks.Archive.Install.run [archive_url, "--sha512", sha512, "--force"]

      nil ->
        Mix.raise "Failed to find installation for Hex #{hex_version} and Elixir #{System.version}"
    end
  end

  defp find_matching_versions_from_signed_csv!(name, path, hex_version) do
    csv = read_path!(name, path)

    signature =
      read_path!(name, path <> ".signed")
      |> String.replace("\n", "")
      |> Base.decode64!()

    if Mix.PublicKey.verify(csv, :sha512, signature) do
      csv
      |> parse_csv()
      |> find_eligible_version(hex_version)
    else
      Mix.raise "Could not install #{name} because Hex could not verify authenticity " <>
                "of metadata file at #{path}. This may happen because a proxy or some " <>
                "entity is interfering with the download or because you don't have a " <>
                "public key to verify the download.\n\nYou may try again later or check " <>
                "if a new public key has been released in our public keys page: #{@public_keys_html}"
    end
  end

  defp read_path!(name, path) do
    case Mix.Utils.read_path(path) do
      {:ok, contents} ->
        contents
      {:remote, message} ->
        Mix.raise """
        #{message}

        Could not install #{name} because Hex could not download metadata at #{path}.
        """
    end
  end

  defp parse_csv(body) do
    body
    |> :binary.split("\n", [:global, :trim])
    |> Enum.map(&:binary.split(&1, ",", [:global, :trim]))
  end

  defp find_eligible_version(entries, hex_version) do
    elixir_version = Hex.Version.parse!(System.version)

    entries
    |> Enum.reverse()
    |> Enum.find_value(&find_version(&1, elixir_version, hex_version))
  end

  defp find_version([hex_version, digest | versions], elixir_version, hex_version) do
    if version = Enum.find(versions, &Version.compare(&1, elixir_version) != :gt) do
      {version, digest}
    end
  end
  defp find_version(_versions, _elixir_version, _hex_version) do
    nil
  end
end
