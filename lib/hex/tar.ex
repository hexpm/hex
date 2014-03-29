defmodule Hex.Tar do
  @version "1"

  def create(meta, files) do
    contents_path = "#{meta[:app]}-#{meta[:version]}-contents.tar.gz"
    path = "#{meta[:app]}-#{meta[:version]}.tar"

    files =
      Enum.map(files, fn
        { name, bin } -> { String.to_char_list!(name), bin }
        name -> name
      end)

    :ok = :erl_tar.create(contents_path, files, [:compressed, :cooked])
    contents = File.read!(contents_path)

    meta_string = Hex.Util.safe_serialize_elixir(meta)
    blob = @version <> meta_string <> contents
    checksum = :crypto.hash(:md5, blob) |> Hex.Util.hexify

    files = [
      { 'VERSION', @version},
      { 'CHECKSUM', checksum },
      { 'metadata.exs', meta_string },
      { 'contents.tar.gz', contents } ]
    :ok = :erl_tar.create(path, files, [:cooked])

    tar = File.read!(path)
    File.rm!(contents_path)
    File.rm!(path)
    tar
  end

  def unpack(path, dest) do
    # TODO: Validate package with checksum, version, etc.
    # note that if tarball is empty :ok is returned, not { :ok, [] }

    case :erl_tar.extract(path, [:memory, files: ['contents.tar.gz']]) do
      { :ok, [{ _name, binary }] } ->
        case :erl_tar.extract({ :binary, binary }, [:compressed, cwd: dest]) do
          :ok ->
            :ok
        { :error, reason } ->
          raise Mix.Error, message: "Unpacking #{path}/contents.tar.gz failed: #{inspect reason}"
        end
      { :error, reason } ->
        raise Mix.Error, message: "Unpacking #{path} failed: #{inspect reason}"
    end
  end
end
