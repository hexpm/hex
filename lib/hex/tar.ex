defmodule Hex.Tar do
  @supported [nil, "2", "3"]
  @version "3"
  @required_files_2 ~w(VERSION CHECKSUM metadata.exs contents.tar.gz)c
  @required_files_3 ~w(VERSION CHECKSUM metadata.config contents.tar.gz)c

  def create(meta, files, cleanup_tarball? \\ true) do
    contents_path = "#{meta[:name]}-#{meta[:version]}-contents.tar.gz"
    path = "#{meta[:name]}-#{meta[:version]}.tar"

    files =
      Enum.map(files, fn
        {name, bin} -> {String.to_char_list(name), bin}
        name -> String.to_char_list(name)
      end)

    :ok = :erl_tar.create(contents_path, files, [:compressed])
    contents = File.read!(contents_path)

    meta_string = encode_term(meta)
    blob = @version <> meta_string <> contents
    checksum = :crypto.hash(:sha256, blob) |> Base.encode16

    files = [
      {'VERSION', @version},
      {'CHECKSUM', checksum},
      {'metadata.config', meta_string},
      {'contents.tar.gz', contents} ]
    :ok = :erl_tar.create(path, files)

    tar = File.read!(path)
    File.rm!(contents_path)
    if cleanup_tarball?, do: File.rm!(path)
    tar
  end

  def unpack(path, dest, {name, version}, lock_checksum) do
    case :erl_tar.extract(path, [:memory]) do
      {:ok, files} ->
        files = Enum.into(files, %{})
        tar_version = files['VERSION']
        check_version(tar_version)
        check_files(tar_version, files)
        checksum(tar_version, files, {name, version}, lock_checksum)
        extract_contents(files['contents.tar.gz'], dest)

      :ok ->
        Mix.raise "Unpacking tarball failed: tarball empty"

      {:error, reason} ->
        Mix.raise "Unpacking tarball failed: " <> format_error(reason)
    end
  end

  defp check_files(version, files) do
    files = Map.keys(files)

    cond do
      version == "2" ->
        diff_files(@required_files_2, files)
      version == "3" ->
        diff_files(@required_files_3, files)
      true ->
        :ok
    end
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

  defp checksum(tar_version, files, {name, version}, lock_checksum) do
    case Base.decode16(files['CHECKSUM'], case: :mixed) do
      {:ok, tar_checksum} ->
        meta              = metadata(tar_version, files)
        blob              = files['VERSION'] <> meta <> files['contents.tar.gz']
        registry_checksum = Hex.Registry.get_checksum(to_string(name), version)
        checksum          = :crypto.hash(:sha256, blob)

        if checksum != tar_checksum,
          do: Mix.raise "Checksum mismatch in tarball"
        if registry_checksum && checksum != Base.decode16!(registry_checksum),
          do: Mix.raise "Checksum mismatch against registry"
        if not is_nil(lock_checksum) and Base.decode16!(lock_checksum, case: :lower) != checksum do
          Mix.raise "Checksum mismatch against lock"
        end

      :error ->
        Mix.raise "Checksum invalid"
    end
  end

  defp extract_contents(file, dest) do
    case :erl_tar.extract({:binary, file}, [:compressed, cwd: dest]) do
      :ok ->
        Path.join(dest, "**")
        |> Path.wildcard
        |> Enum.each(&File.touch!/1)
        :ok
      {:error, reason} ->
        Mix.raise "Unpacking inner tarball failed: " <> format_error(reason)
    end
  end

  defp metadata("2", files), do: files['metadata.exs']
  defp metadata("3", files), do: files['metadata.config']

  defp encode_term(list) do
    list
    |> Hex.Utils.binarify(maps: false)
    |> Enum.map(&[:io_lib_pretty.print(&1, encoding: :utf8) | ".\n"])
    |> IO.chardata_to_string
  end

  defp format_error({_path, reason}) do
    format_error(reason)
  end

  defp format_error(reason) do
    :erl_tar.format_error(reason)
    |> List.to_string
  end
end
