defmodule Hex.Tar do
  @moduledoc false

  # TODO: convert strings to atoms when unpacking
  #
  # @type metadata() :: %{
  #         app: String.t(),
  #         version: String.t(),
  #         app: String.t(),
  #         description: String.t(),
  #         files: String.t(),
  #         licenses: String.t(),
  #         requirements: [requirement()],
  #         build_tools: list(String.t()),
  #         elixir: version_requirement(),
  #         maintainers: list(String.t()),
  #         links: map()
  #       }
  #
  # @type requirement() :: %{
  #         app: String.t(),
  #         optional: boolean(),
  #         requirement: version_requirement(),
  #         repository: String.t()
  #       }

  @type metadata() :: map()

  @type file() :: Path.t() | {Path.t(), binary()}

  @type checksum :: binary()

  @type tar :: binary()

  @supported ["3"]
  @version "3"
  @required_files ~w(VERSION CHECKSUM metadata.config contents.tar.gz)c

  @doc """
  Creates a package tarball.

  ## Examples

      iex> {:ok, {tar, checksum}} = Hex.Tar.create(%{app: :ecto, version: "2.0.0"}, ["lib/ecto.ex"], "deps/")
      # creates deps/ecto-2.0.0.tar

      iex> {:ok, {tar, checksum}} = Hex.Tar.create(%{app: :ecto, version: "2.0.0"}, [{"lib/ecto.ex", "defmodule Ecto"}], ".")
      # creates ./ecto-2.0.0.tar

      iex> {:ok, {tar, checksum}} = Hex.Tar.create(%{app: :ecto, version: "2.0.0"}, [{"ecto.ex", "defmodule Ecto"}], :memory)
      # nothing is saved to disk

  """
  @spec create(metadata(), [file()], output :: Path.t() | :memory) ::
          {:ok, {tar(), checksum()}} | {:error, term()}
  def create(meta, files, output) do
    contents = create_tar(:memory, files, [:compressed])

    meta_string = encode_term(meta)
    blob = @version <> meta_string <> contents
    checksum = :crypto.hash(:sha256, blob)

    files = [
      {"VERSION", @version},
      {"CHECKSUM", Base.encode16(checksum)},
      {"metadata.config", meta_string},
      {"contents.tar.gz", contents}
    ]

    tar = create_tar(output, files, [])
    {:ok, {tar, checksum}}
  end

  def create_tar(path, files, opts) when is_binary(path) do
    File.mkdir_p!(Path.dirname(path))
    {:ok, tar} = :hex_erl_tar.open(string_to_charlist(path), opts ++ [:write])

    try do
      add_files(tar, files)
    after
      :hex_erl_tar.close(tar)
    end

    File.read!(path)
  end

  def create_tar(:memory, files, opts) do
    compressed? = :compressed in opts
    {:ok, fd} = :file.open([], [:ram, :read, :write, :binary])
    {:ok, tar} = :hex_erl_tar.init(fd, :write, &file_op/2)

    binary =
      try do
        try do
          add_files(tar, files)
        after
          :ok = :hex_erl_tar.close(tar)
        end

        {:ok, size} = :file.position(fd, :cur)
        {:ok, binary} = :file.pread(fd, 0, size)
        binary
      after
        :ok = :file.close(fd)
      end

    if compressed? do
      :zlib.gzip(binary)
    else
      binary
    end
  end

  defp file_op(:write, {fd, data}), do: :file.write(fd, data)
  defp file_op(:position, {fd, pos}), do: :file.position(fd, pos)
  defp file_op(:read2, {fd, size}), do: :file.read(fd, size)
  defp file_op(:close, _fd), do: :ok

  defp add_files(tar, files) do
    Enum.each(files, fn
      {name, contents, mode} ->
        :ok = :hex_erl_tar.add(tar, contents, string_to_charlist(name), mode, [])

      {name, contents} ->
        :ok = :hex_erl_tar.add(tar, contents, string_to_charlist(name), [])

      name ->
        case file_lstat(name) do
          {:ok, %File.Stat{type: type}} when type in [:directory, :symlink] ->
            :ok = :hex_erl_tar.add(tar, string_to_charlist(name), [])

          _stat ->
            contents = File.read!(name)
            mode = File.stat!(name).mode
            :ok = :hex_erl_tar.add(tar, contents, string_to_charlist(name), mode, [])
        end
    end)
  end

  @doc """
  Unpacks a package tarball.

  ## Examples

      iex> {:ok, {metadata, checksum}} = Hex.Tar.unpack("ecto-2.0.0.tar", "deps/")
      # unpacks to deps/ecto-2.0.0/

      iex> {:ok, {metadata, checksum, files}} = Hex.Tar.unpack({:binary, tar}, :memory)
      iex> files
      [{"lib/ecto.ex", "defmodule Ecto ..."}, ...]

  """
  @spec unpack(file :: Path.t() | {:binary, binary()}, output :: Path.t()) ::
          {:ok, {metadata(), checksum()}} | {:error, term()}
  @spec unpack(file :: Path.t() | {:binary, binary()}, output :: :memory) ::
          {:ok, {metadata(), checksum(), files :: [{Path.t(), binary()}]}} | {:error, term()}
  def unpack(tar, dest) do
    case :hex_erl_tar.extract(tar, [:memory]) do
      {:ok, files} when files != [] ->
        files = Enum.into(files, %{})

        %{checksum: nil, files: files, metadata: nil, contents: nil}
        |> check_files()
        |> check_version()
        |> check_checksum()
        |> copy_metadata(dest)
        |> decode_metadata()
        |> extract_contents(dest)

      {:ok, []} ->
        {:error, {:tarball, :empty}}

      {:error, reason} ->
        {:error, {:tarball, reason}}
    end
  end

  defp check_version({:error, _} = error), do: error

  defp check_version(state) do
    version = state.files['VERSION']

    if version in @supported do
      state
    else
      {:error, {:unsupported_version, version}}
    end
  end

  defp check_files({:error, _} = error), do: error

  defp check_files(state) do
    case @required_files -- Map.keys(state.files) do
      [] -> state
      diff -> {:error, {:missing_files, diff}}
    end
  end

  defp check_checksum({:error, _} = error), do: error

  defp check_checksum(state) do
    checksum_base16 = state.files['CHECKSUM']

    case Base.decode16(checksum_base16, case: :mixed) do
      {:ok, expected_checksum} ->
        meta = state.files['metadata.config']
        blob = state.files['VERSION'] <> meta <> state.files['contents.tar.gz']
        actual_checksum = :crypto.hash(:sha256, blob)

        if expected_checksum == actual_checksum do
          %{state | checksum: expected_checksum}
        else
          {:error, {:checksum_mismatch, expected_checksum, actual_checksum}}
        end

      :error ->
        {:error, :invalid_checksum}
    end
  end

  defp decode_metadata({:error, _} = error), do: error

  defp decode_metadata(state) do
    string = safe_to_charlist(state.files['metadata.config'])

    case :safe_erl_term.string(string) do
      {:ok, tokens, _line} ->
        try do
          terms = :safe_erl_term.terms(tokens)
          %{state | metadata: Enum.into(terms, %{})}
        rescue
          FunctionClauseError ->
            {:error, {:metadata, :invalid_terms}}

          ArgumentError ->
            {:error, {:metadata, :not_key_value}}
        end

      {:error, {_line, :safe_erl_term, reason}, _line2} ->
        {:error, {:metadata, reason}}
    end
  end

  defp copy_metadata({:error, _} = error, _dest), do: error

  defp copy_metadata(state, :memory) do
    state
  end

  defp copy_metadata(state, dest) do
    File.mkdir_p!(dest)
    file_name = "hex_metadata.config"
    path = Path.join(dest, file_name)
    File.write!(path, state.files['metadata.config'])
    state
  end

  defp extract_contents({:error, _} = error, _dest), do: error

  defp extract_contents(state, dest) do
    case do_extract_contents(state.files['contents.tar.gz'], dest) do
      :ok ->
        {:ok, {state.metadata, state.checksum}}

      {:ok, files} ->
        files = for {path, contents} <- files, do: {List.to_string(path), contents}
        {:ok, {state.metadata, state.checksum, files}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_extract_contents(binary, :memory) do
    case :hex_erl_tar.extract({:binary, binary}, [:compressed, :memory]) do
      {:ok, files} ->
        {:ok, files}

      {:error, reason} ->
        {:error, {:inner_tarball, reason}}
    end
  end

  defp do_extract_contents(binary, dest) do
    case :hex_erl_tar.extract({:binary, binary}, [:compressed, cwd: dest]) do
      :ok ->
        Path.join(dest, "**")
        |> Path.wildcard()
        |> Enum.each(&File.touch!/1)

        :ok

      {:error, reason} ->
        {:error, {:inner_tarball, reason}}
    end
  end

  defp encode_term(list) do
    list
    |> binarify(maps: false)
    |> Enum.map(&[:io_lib_pretty.print(&1, encoding: :utf8) | ".\n"])
    |> IO.chardata_to_string()
  end

  @tarball_error "Unpacking tarball failed: "
  @inner_error "Unpacking inner tarball failed: "
  @metadata_error "Error reading package metadata: "

  def format_error({:tarball, :empty}), do: @tarball_error <> "Empty tarball"
  def format_error({:tarball, reason}), do: @tarball_error <> format_tarball_error(reason)
  def format_error({:inner_tarball, reason}), do: @inner_error <> format_tarball_error(reason)
  def format_error({:metadata, :invalid_terms}), do: @metadata_error <> "Invalid terms"
  def format_error({:metadata, :not_key_value}), do: @metadata_error <> "Not in key-value format"
  def format_error({:metadata, reason}), do: @metadata_error <> format_metadata_error(reason)
  def format_error(:invalid_checksum), do: "Invalid tarball checksum"

  def format_error({:checksum_mismatch, expected_checksum, actual_checksum}) do
    "Tarball checksum mismatch\n\n" <>
      "Expected (base16-encoded): #{Base.encode16(expected_checksum)}\n" <>
      "Actual   (base16-encoded): #{Base.encode16(actual_checksum)}"
  end

  defp format_tarball_error(reason) do
    reason |> :hex_erl_tar.format_error() |> List.to_string()
  end

  defp format_metadata_error(reason) do
    reason |> :safe_erl_term.format_error() |> List.to_string()
  end

  # Utils

  # Some older packages have invalid unicode
  defp safe_to_charlist(string) do
    try do
      string_to_charlist(string)
    rescue
      UnicodeConversionError ->
        :erlang.binary_to_list(string)
    end
  end

  if Version.compare(System.version(), "1.3.0") == :lt do
    defp string_to_charlist(string), do: String.to_char_list(string)
  else
    defp string_to_charlist(string), do: String.to_charlist(string)
  end

  defp binarify(binary, _opts) when is_binary(binary) do
    binary
  end

  defp binarify(number, _opts) when is_number(number) do
    number
  end

  defp binarify(atom, _opts) when is_nil(atom) or is_boolean(atom) do
    atom
  end

  defp binarify(atom, _opts) when is_atom(atom) do
    Atom.to_string(atom)
  end

  defp binarify(list, opts) when is_list(list) do
    for(elem <- list, do: binarify(elem, opts))
  end

  defp binarify(tuple, opts) when is_tuple(tuple) do
    for(elem <- Tuple.to_list(tuple), do: binarify(elem, opts))
    |> List.to_tuple()
  end

  defp binarify(map, opts) when is_map(map) do
    if Keyword.get(opts, :maps, true) do
      for(elem <- map, into: %{}, do: binarify(elem, opts))
    else
      for(elem <- map, do: binarify(elem, opts))
    end
  end

  defp file_lstat(path, opts \\ []) do
    opts = Keyword.put_new(opts, :time, :universal)

    case :file.read_link_info(IO.chardata_to_string(path), opts) do
      {:ok, fileinfo} ->
        {:ok, File.Stat.from_record(fileinfo)}

      error ->
        error
    end
  end
end
