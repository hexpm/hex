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
        {filename, path} when is_list(path) -> {String.to_charlist(filename), path}
        {filename, path, options} -> {String.to_charlist(filename), path, options}
        {filename, contents} when is_binary(contents) -> {String.to_charlist(filename), contents}
        filename -> String.to_charlist(filename)
      end)

    config =
      :mix_hex_core.default_config()
      |> Map.put(:tarball_files_root, File.cwd!() |> String.to_charlist())

    case :mix_hex_tarball.create(metadata, files, config) do
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
        _ -> {:file, String.to_charlist(path)}
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
    case :file.open(path, [:read, :raw, :binary]) do
      {:ok, file} ->
        try do
          hash_file(file, :crypto.hash_init(:sha256))
        after
          :file.close(file)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp hash_file(file, hash) do
    case :file.read(file, 65_536) do
      {:ok, data} -> hash_file(file, :crypto.hash_update(hash, data))
      :eof -> {:ok, :crypto.hash_final(hash)}
      {:error, reason} -> {:error, reason}
    end
  end
end
