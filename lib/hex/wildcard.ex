defmodule Hex.Wildcard do
  @moduledoc false

  # Expands package file patterns with :filelib.wildcard/2, which calls
  # list_dir/1, read_file_info/1 and read_link_info/1 of this module to access
  # the file system. Names are given to the file system as binaries, which are
  # the raw bytes of the name with both the :utf8 and :latin1 native name
  # encodings, and to filelib as Unicode characters, so a pattern matches the
  # same files with both encodings. Names that are not valid UTF-8 raise in
  # every directory filelib lists, Path.wildcard/1 skips them with the :utf8
  # encoding and returns them with the :latin1 encoding.

  @doc """
  Returns the paths matching the pattern, spelled as they are on disk.

  Only the directories filelib lists are compared to the pattern, a literal
  path is checked by looking it up. On case-insensitive file systems the
  lookup finds names with a different case and filelib returns them spelled
  as in the pattern, so only paths where every component is an entry of its
  parent directory are kept. Names are compared in NFC because macOS returns
  them normalized with the :utf8 native encoding.
  """
  def wildcard(pattern) do
    paths =
      pattern
      |> String.to_charlist()
      |> :filelib.wildcard(__MODULE__)
      |> Enum.map(&List.to_string/1)
      |> Enum.map(&{&1, path_steps(&1)})

    dirs = for {_path, steps} <- paths, {dir, _name} <- steps, uniq: true, do: dir
    listings = Map.new(dirs, &{&1, dir_names(&1)})

    for {path, steps} <- paths,
        Enum.all?(steps, fn {dir, name} -> nfc(name) in listings[dir] end),
        do: path
  end

  @doc """
  Returns the names in a directory as binaries with the bytes of the names on
  disk.
  """
  def list_names(dir) do
    case :file.list_dir_all(dir) do
      {:ok, names} -> {:ok, Enum.map(names, &raw_name/1)}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Returns the names in a directory like `list_names/1`, raises if the directory
  can't be listed or a name is not valid UTF-8.
  """
  def list_names!(dir) do
    case list_names(dir) do
      {:ok, names} ->
        Enum.each(names, &valid_name!(join_path(dir, &1)))
        names

      {:error, reason} ->
        raise File.Error, reason: reason, action: "list directory", path: dir
    end
  end

  @doc false
  def list_dir(dir) do
    dir = List.to_string(dir)

    case list_names(dir) do
      {:ok, names} ->
        Enum.each(names, &valid_name!(join_path(dir, &1)))
        names = Enum.reject(names, &String.starts_with?(&1, "."))
        {:ok, Enum.map(names, &String.to_charlist/1)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc false
  def read_file_info(file), do: :file.read_file_info(List.to_string(file))

  @doc false
  def read_link_info(file), do: :file.read_link_info(List.to_string(file))

  @doc false
  def join_path(dir, name) do
    case Enum.reject(Path.split(dir), &(&1 == ".")) do
      [] -> name
      parts -> Path.join(parts ++ [name])
    end
  end

  defp raw_name(name) when is_binary(name), do: name

  defp raw_name(name) do
    case :file.native_name_encoding() do
      :utf8 -> List.to_string(name)
      :latin1 -> :erlang.list_to_binary(name)
    end
  end

  defp valid_name!(path) do
    if not String.valid?(path) do
      Mix.raise(
        "Can't build package when file name is not valid UTF-8: " <>
          inspect(path, binaries: :as_strings)
      )
    end
  end

  defp path_steps(path) do
    {steps, _dir} =
      path
      |> Path.split()
      |> Enum.map_reduce(".", fn name, dir -> {{dir, name}, join_path(dir, name)} end)

    Enum.reject(steps, fn {_dir, name} -> name in [".", ".."] end)
  end

  defp dir_names(dir) do
    case list_names(dir) do
      {:ok, names} -> MapSet.new(names, &nfc/1)
      {:error, _reason} -> MapSet.new()
    end
  end

  defp nfc(name) do
    case :unicode.characters_to_nfc_binary(name) do
      normalized when is_binary(normalized) -> normalized
      _error -> name
    end
  end
end
