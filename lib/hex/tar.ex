defmodule Hex.Tar do
  @moduledoc false

  def create!(_metadata, [], _output),
    do:
      Mix.raise(
        "Stopping package build due to errors.\nCreating tarball failed: File list was empty."
      )

  def create!(metadata, files, output) do
    files =
      Enum.map(files, fn
        {filename, contents} -> {String.to_charlist(filename), contents}
        filename -> String.to_charlist(filename)
      end)

    case :mix_hex_tarball.create(metadata, files) do
      {:ok, %{tarball: tarball} = result} ->
        if output != :memory, do: File.write!(output, tarball)
        result

      {:error, reason} ->
        Mix.raise("Creating tarball failed: #{:mix_hex_tarball.format_error(reason)}")
    end
  end

  def unpack!(path, dest) do
    tarball =
      case path do
        {:binary, tarball} -> tarball
        _ -> File.read!(path)
      end

    dest = if dest == :memory, do: dest, else: String.to_charlist(dest)

    case :mix_hex_tarball.unpack(tarball, dest) do
      {:ok, result} ->
        result

      {:error, reason} ->
        Mix.raise("Unpacking tarball failed: #{:mix_hex_tarball.format_error(reason)}")
    end
  end

  # TODO: Add this function to
  def outer_checksum(path) do
    case File.read(path) do
      {:ok, tarball} -> {:ok, :crypto.hash(:sha256, tarball)}
      {:error, reason} -> {:error, reason}
    end
  end
end
