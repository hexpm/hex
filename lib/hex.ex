defmodule Hex do
  @moduledoc false

  def start() do
    {:ok, _} = Application.ensure_all_started(:hex)
  end

  def stop() do
    case Application.stop(:hex) do
      :ok -> :ok
      {:error, {:not_started, :hex}} -> :ok
    end
  end

  # For compatability during development
  def start(start_type, start_args) do
    Hex.Application.start(start_type, start_args)
  end

  def version(), do: unquote(Mix.Project.config()[:version])
  def elixir_version(), do: unquote(System.version())
  def otp_version(), do: unquote(Hex.Utils.otp_version())

  if Version.compare(System.version(), "1.2.0") == :lt do
    def debug?(), do: false
  else
    def debug?(), do: Mix.debug?()
  end

  if Version.compare(System.version(), "1.3.0") == :lt do
    def string_trim(string), do: String.strip(string)
    def to_charlist(term), do: Kernel.to_char_list(term)
    def string_to_charlist(string), do: String.to_char_list(string)

    def string_trim_leading(string, trim) do
      trim_size = byte_size(trim)

      case string do
        <<^trim::binary-size(trim_size), rest::binary>> -> string_trim_leading(rest, trim)
        _other -> string
      end
    end

    def string_trim_trailing(string, trim) do
      rest_size = byte_size(string) - byte_size(trim)

      case string do
        <<rest::binary-size(rest_size), ^trim::binary>> -> string_trim_trailing(rest, trim)
        _other -> string
      end
    end

    def string_pad_trailing(string, count) do
      filler_size = max(count - byte_size(string), 0)
      string <> String.duplicate(" ", filler_size)
    end
  else
    def string_trim(string), do: String.trim(string)
    def string_trim_leading(string, trim), do: String.trim_leading(string, trim)
    def string_trim_trailing(string, trim), do: String.trim_trailing(string, trim)
    def string_pad_trailing(string, count), do: String.pad_trailing(string, count)
    def to_charlist(term), do: Kernel.to_charlist(term)
    def string_to_charlist(string), do: String.to_charlist(string)
  end

  if Version.compare(System.version(), "1.3.2") == :lt do
    def check_deps(), do: Mix.Tasks.Deps.Check.run(["--no-compile"])
  else
    def check_deps(), do: Mix.Tasks.Deps.Loadpaths.run(["--no-compile"])
  end

  if Version.compare(System.version(), "1.4.0") == :lt do
    def enum_split_with(enum, fun), do: Enum.partition(enum, fun)
  else
    def enum_split_with(enum, fun), do: Enum.split_with(enum, fun)
  end

  def file_lstat(path, opts \\ []) do
    opts = Keyword.put_new(opts, :time, :universal)

    case :file.read_link_info(IO.chardata_to_string(path), opts) do
      {:ok, fileinfo} ->
        {:ok, File.Stat.from_record(fileinfo)}

      error ->
        error
    end
  end

  def file_lstat!(path, opts \\ []) do
    case file_lstat(path, opts) do
      {:ok, info} ->
        info

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "read file stats",
          path: IO.chardata_to_string(path)
    end
  end

  def create_tar!(_metadata, [], _output),
    do:
      Mix.raise(
        "Stopping package build due to errors.\nCreating tarball failed: File list was empty."
      )

  def create_tar!(metadata, files, output) do
    files =
      Enum.map(files, fn
        {filename, contents} -> {string_to_charlist(filename), contents}
        filename -> string_to_charlist(filename)
      end)

    case :mix_hex_tarball.create(metadata, files) do
      {:ok, %{tarball: tarball} = result} ->
        if output != :memory, do: File.write!(output, tarball)
        result

      {:error, reason} ->
        Mix.raise("Creating tarball failed: #{:mix_hex_tarball.format_error(reason)}")
    end
  end

  def unpack_tar!(path, dest) do
    tarball =
      case path do
        {:binary, tarball} -> tarball
        _ -> File.read!(path)
      end

    dest = if dest == :memory, do: dest, else: string_to_charlist(dest)

    case :mix_hex_tarball.unpack(tarball, dest) do
      {:ok, result} ->
        result

      {:error, reason} ->
        Mix.raise("Unpacking tarball failed: #{:mix_hex_tarball.format_error(reason)}")
    end
  end

  def filename_matches_semver?(filename) do
    case Version.parse(to_string(filename)) do
      {:ok, _struct} -> true
      _ -> false
    end
  end

  def semver_error_text do
    "Invalid filename: top-level filenames cannot match a semantic version pattern."
  end
end
