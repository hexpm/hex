defmodule Hex.Tar do
  @supported ["2", "3"]
  @version "2"
  @required_files_2 ~w(VERSION CHECKSUM metadata.exs contents.tar.gz)c
  @required_files_3 ~w(VERSION CHECKSUM metadata.config contents.tar.gz)c

  def create(meta, files) do
    contents_path = "#{meta[:name]}-#{meta[:version]}-contents.tar.gz"
    path = "#{meta[:name]}-#{meta[:version]}.tar"

    files =
      Enum.map(files, fn
        {name, bin} -> {String.to_char_list(name), bin}
        name -> String.to_char_list(name)
      end)

    :ok = :erl_tar.create(contents_path, files, [:compressed])
    contents = File.read!(contents_path)

    meta_string = Hex.Util.safe_serialize_elixir(meta)
    blob = @version <> meta_string <> contents
    checksum = :crypto.hash(:sha256, blob) |> Base.encode16

    files = [
      {'VERSION', @version},
      {'CHECKSUM', checksum},
      {'metadata.exs', meta_string},
      {'contents.tar.gz', contents}]
    :ok = :erl_tar.create(path, files)

    tar = File.read!(path)
    File.rm!(contents_path)
    File.rm!(path)
    tar
  end

  def unpack(path, dest, {name, version}) do
    case :erl_tar.extract(path, [:memory]) do
      {:ok, files} ->
        files = Enum.into(files, %{})
        version = files['VERSION']
        check_version(version, path)
        check_files(version, files, path)
        checksum(version, files, path, {name, version})
        extract_contents(files['contents.tar.gz'], dest, path)

      :ok ->
        Mix.raise "Unpacking #{path} failed: tarball empty"

      {:error, reason} ->
        Mix.raise "Unpacking #{path} failed: #{inspect reason}"
    end
  end

  defp check_files("2", files, path) do
    check_required_files(@required_files_2, files, path)
  end

  defp check_files("3", files, path) do
    check_required_files(@required_files_3, files, path)
  end

  defp check_required_files(required_files, files, path) do
    diff = required_files -- Dict.keys(files)
    if diff != [] do
      diff = Enum.join(diff, ", ")
      Mix.raise "Missing files #{diff} in #{path}"
    end
  end

  defp check_version(version, path) do
    unless version in @supported do
      raise Mix.Error,
        message: "Unsupported tarball version #{version} in #{path}. " <>
                 "Try updating Hex with `mix local.hex`."
    end
  end

  defp checksum(version, files, path, {name, version}) do
    case Base.decode16(files['CHECKSUM'], case: :mixed) do
      {:ok, tar_checksum} ->
        blob = files['VERSION'] <> metadata(version, files) <> files['contents.tar.gz']
        registry_checksum = Hex.Registry.get_checksum(to_string(name), version)
        checksum = :crypto.hash(:sha256, blob)

        if checksum != tar_checksum do
          Mix.raise "Checksum mismatch against tarball in #{path}"
        end

        if registry_checksum && checksum != Base.decode16!(registry_checksum) do
          Mix.raise "Checksum mismatch against registry in #{path}"
        end

      :error ->
        Mix.raise "Checksum invalid in #{path}"
    end
  end

  defp extract_contents(file, dest, path) do
    case :erl_tar.extract({:binary, file}, [:compressed, cwd: dest]) do
      :ok ->
        Path.join(dest, "**")
        |> Path.wildcard
        |> Enum.each(&File.touch!/1)
        :ok
      {:error, reason} ->
        Mix.raise "Unpacking #{path}/contents.tar.gz failed: #{inspect reason}"
    end
  end

  defp metadata("2", files), do: files['metadata.exs']
  defp metadata("3", files), do: files['metadata.config']
end
