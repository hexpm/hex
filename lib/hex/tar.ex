defmodule Hex.Tar do
  @supported ["3"]
  @version "3"
  @required_files ~w(VERSION CHECKSUM metadata.config contents.tar.gz)c

  def create(meta, files, opts \\ []) do
    output = Keyword.get(opts, :output, "#{meta[:name]}-#{meta[:version]}.tar")
    cleanup_tarball? = Keyword.get(opts, :cleanup_tarball?, true)

    contents_path = "#{meta[:name]}-#{meta[:version]}-contents.tar.gz"

    :ok = create_tar(contents_path, files, [:compressed])
    contents = File.read!(contents_path)

    meta_string = encode_term(meta)
    blob = @version <> meta_string <> contents
    checksum = :crypto.hash(:sha256, blob) |> Base.encode16()

    files = [
      {"VERSION", @version},
      {"CHECKSUM", checksum},
      {"metadata.config", meta_string},
      {"contents.tar.gz", contents}
    ]
    :ok = create_tar(output, files, [])

    tar = File.read!(output)
    File.rm!(contents_path)
    if cleanup_tarball?, do: File.rm!(output)
    {tar, checksum}
  end

  defp create_tar(path, files, opts) do
    {:ok, tar} = :hex_erl_tar.open(Hex.string_to_charlist(path), opts ++ [:write])

    try do
      Enum.each(files, fn
        {name, contents, mode} ->
          :ok = :hex_erl_tar.add(tar, contents, Hex.string_to_charlist(name), mode, [])
        {name, contents} ->
          :ok = :hex_erl_tar.add(tar, contents, Hex.string_to_charlist(name), [])
        name ->
          case Hex.file_lstat(name) do
            {:ok, %File.Stat{type: type}} when type in [:directory, :symlink] ->
              :ok = :hex_erl_tar.add(tar, Hex.string_to_charlist(name), [])
            _stat ->
              contents = File.read!(name)
              mode = File.stat!(name).mode
              :ok = :hex_erl_tar.add(tar, contents, Hex.string_to_charlist(name), mode, [])
          end
      end)
    after
      :hex_erl_tar.close(tar)
    end
  end

  @spec unpack(Path.t() | {:binary, binary}, Path.t()) :: {map(), String.t()}
  def unpack(tar, dest) do
    case :hex_erl_tar.extract(tar, [:memory]) do
      {:ok, files} ->
        files = Enum.into(files, %{})
        check_version(files['VERSION'])
        check_files(files)
        checksum = checksum(files)
        extract_contents(files['contents.tar.gz'], dest)
        copy_metadata(files['metadata.config'], dest)
        {decode_metadata(files['metadata.config']), checksum}

      :ok ->
        Mix.raise "Unpacking tarball failed: tarball empty"

      {:error, reason} ->
        Mix.raise "Unpacking tarball failed: " <> format_error(reason)
    end
  end

  defp check_files(files) do
    files = Map.keys(files)
    diff_files(@required_files, files)
  end

  defp diff_files(required, given) do
    diff = required -- given
    if diff != [] do
      diff = Enum.join(diff, ", ")
      Mix.raise "Missing files in tarball #{diff}"
    end
  end

  defp check_version(version) do
    unless version in @supported do
      Mix.raise "Unsupported tarball version #{version}. " <>
                 "Try updating Hex with `mix local.hex`."
    end
  end

  defp checksum(files) do
    case Base.decode16(files['CHECKSUM'], case: :mixed) do
      {:ok, tar_checksum} ->
        meta = files['metadata.config']
        blob = files['VERSION'] <> meta <> files['contents.tar.gz']
        checksum = :crypto.hash(:sha256, blob)

        if checksum == tar_checksum do
          checksum
        else
          Mix.raise "Checksum mismatch in tarball"
        end

      :error ->
        Mix.raise "Checksum invalid"
    end
  end

  def extract_contents(file, dest, opts \\ []) do
    mode = opts[:mode] || :binary
    case :hex_erl_tar.extract({mode, file}, [:compressed, cwd: dest]) do
      :ok ->
        Path.join(dest, "**")
        |> Path.wildcard()
        |> Enum.each(&File.touch!/1)
        :ok
      {:error, reason} ->
        Mix.raise "Unpacking inner tarball failed: " <> format_error(reason)
    end
  end

  defp encode_term(list) do
    list
    |> Hex.Utils.binarify(maps: false)
    |> Enum.map(&[:io_lib_pretty.print(&1, encoding: :utf8) | ".\n"])
    |> IO.chardata_to_string()
  end

  defp format_error({_path, reason}) do
    format_error(reason)
  end

  defp format_error(reason) do
    :hex_erl_tar.format_error(reason)
    |> List.to_string()
  end

  defp decode_metadata(contents) do
    string = safe_to_charlist(contents)
    case :safe_erl_term.string(string) do
      {:ok, tokens, _line} ->
        try do
          terms = :safe_erl_term.terms(tokens)
          Enum.into(terms, %{})
        rescue
          FunctionClauseError ->
            Mix.raise "Error reading package metadata: invalid terms"
          ArgumentError ->
            Mix.raise "Error reading package metadata: not in key-value format"
        end

      {:error, reason} ->
        Mix.raise "Error reading package metadata: #{inspect reason}"
    end
  end

  defp copy_metadata(content, dest) do
    file_name = "hex_metadata.config"
    path = Path.join(dest, file_name)
    File.write!(path, content)
  end

  # Some older packages have invalid unicode
  defp safe_to_charlist(string) do
    try do
      Hex.string_to_charlist(string)
    rescue
      UnicodeConversionError ->
        :erlang.binary_to_list(string)
    end
  end
end
