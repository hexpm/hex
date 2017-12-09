defmodule Hex.TarTest do
  use HexTest.Case
  @moduletag :integration

  @mix_exs """
  defmodule Foo.MixProject do
    use Mix.Project

    def project do
      [
        app: :foo,
        version: "0.1.0",
        elixir: "~> 1.0"
      ]
    end
  end
  """

  @metadata %{
    name: "foo",
    version: "0.1.0",
    app: "foo",
    requirements: %{
      bar: %{
        app: :bar, optional: "false", requirement: "0.1.0"
      }
    }
  }

  @files ["mix.exs"]

  test "create and unpack on disk" do
    in_tmp(fn ->
      File.write!("mix.exs", @mix_exs)

      assert {:ok, {tar, checksum}} = Hex.Tar.create(@metadata, @files, "a/b.tar")
      assert {:ok, {metadata, ^checksum}} = Hex.Tar.unpack("a/b.tar", "unpack/")

      assert byte_size(checksum) == 32
      assert File.read!("a/b.tar") == tar
      assert File.ls!("unpack") == ["hex_metadata.config", "mix.exs"]
      assert {:ok, metadata_kw} = :file.consult("unpack/hex_metadata.config")
      assert :proplists.get_keys(metadata_kw) == ~w(name app version requirements)
      assert File.read!("unpack/mix.exs") == File.read!("mix.exs")
      assert metadata == stringify(@metadata)
    end)
  end

  test "create and unpack in-memory" do
    in_tmp(fn ->
      files = [{"mix.exs", @mix_exs}]

      assert {:ok, {tar, checksum}} = Hex.Tar.create(@metadata, files, :memory)
      assert {:ok, {metadata2, ^checksum, ^files}} = Hex.Tar.unpack({:binary, tar}, :memory)
      assert metadata2 == stringify(@metadata)
    end)
  end

  test "create in-memory matches saving to file" do
    in_tmp(fn ->
      File.write!("mix.exs", @mix_exs)

      assert {:ok, {tar, checksum}} = Hex.Tar.create(@metadata, @files, "a.tar")
      assert {:ok, {^tar, ^checksum}} = Hex.Tar.create(@metadata, @files, :memory)

      assert byte_size(tar) == 5120
    end)
  end

  test "unpack optional fields" do
  end

  test "unpack with legacy requirements format" do
    in_tmp(fn ->
      files = [{"mix.exs", @mix_exs}]
      {:ok, {tar, _checksum}} = Hex.Tar.create(@metadata, files, :memory)
      {:ok, files} = :hex_erl_tar.extract({:binary, tar}, [:memory])

      files =
        files
        |> replace_file('CHECKSUM', "22D7C2C004D6096D8B4BB40D984782D4D9D97E059F38632A947B4550C28A2B4A")
        |> replace_file('metadata.config', """
                        {<<\"app\">>,<<\"foo\">>}.
                        {<<\"name\">>,<<\"foo\">>}.
                        {<<\"version\">>,<<\"0.1.0\">>}.
                        {<<\"requirements\">>,[
                          [
                            {<<\"name\">>,<<\"bar\">>},
                            {<<\"app\">>,<<\"bar\">>},
                            {<<\"requirement\">>,<<\"~> 0.1.0\">>},
                            {<<\"optional\">>,false}]
                          ]
                        }.
                        """)

      :ok = :hex_erl_tar.create('legacy_requirements.tar', files, [:write])
      assert {:ok, {metadata, _checksum, _files}} = Hex.Tar.unpack("legacy_requirements.tar", :memory)
      assert metadata["requirements"] == %{
               "bar" => %{
                 "app" => "bar",
                 "optional" => false,
                 "requirement" => "~> 0.1.0"
               }
             }
    end)
  end

  test "unpack error handling" do
    in_tmp(fn ->
      files = [{"mix.exs", @mix_exs}]
      {:ok, {tar, _checksum}} = Hex.Tar.create(@metadata, files, :memory)
      {:ok, valid_files} = :hex_erl_tar.extract({:binary, tar}, [:memory])

      # tarball
      assert {:error, {:tarball, :eof}} = Hex.Tar.unpack({:binary, "badtar"}, :memory)

      assert {:error, {:tarball, {"missing.tar", :enoent}}} = Hex.Tar.unpack("missing.tar", :memory)

      :ok = :hex_erl_tar.create('empty.tar', [], [:write])
      assert {:error, {:tarball, :empty}} = Hex.Tar.unpack("empty.tar", :memory)

      :ok = :hex_erl_tar.create('missing.tar', [{'VERSION', "3"}, {'ignored', "IGNORED"}], [:write])
      assert {:error, {:missing_files, ['CHECKSUM' | _]}} = Hex.Tar.unpack("missing.tar", :memory)

      files = replace_file(valid_files, 'VERSION', "0")
      :ok = :hex_erl_tar.create('badversion.tar', files, [:write])
      assert {:error, {:unsupported_version, "0"}} = Hex.Tar.unpack("badversion.tar", :memory)

      # checksum
      files = replace_file(valid_files, 'CHECKSUM', "bad")
      :ok = :hex_erl_tar.create('badchecksum.tar', files, [:write])
      assert {:error, :invalid_checksum} = Hex.Tar.unpack("badchecksum.tar", :memory)

      files = replace_file(valid_files, 'metadata.config', "{<<\"name\">>,<<\"foo\">>}")
      :ok = :hex_erl_tar.create('checksummismatch.tar', files, [:write])
      assert {:error, {:checksum_mismatch, _, _}} = Hex.Tar.unpack("checksummismatch.tar", :memory)

      # metadata
      files =
        valid_files
        |> replace_file('metadata.config', "ok $")
        |> replace_file('CHECKSUM', "7CC4126B1B4DA063841229BA8952AA4BAA4F21615F8CBD69DA8B36C0733F7151")

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])
      assert {:error, {:metadata, {:illegal, '$'}}} = Hex.Tar.unpack("badmetadata.tar", :memory)

      files =
        valid_files
        |> replace_file('metadata.config', "ok[")
        |> replace_file('CHECKSUM', "7E65497E24EB8D39D7A2882928DF254D2CF6C8950998C651B88D3F12EB3D2152")

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])
      assert {:error, {:metadata, :invalid_terms}} = Hex.Tar.unpack("badmetadata.tar", :memory)

      files =
        valid_files
        |> replace_file('metadata.config', "asdfasdfasdfasdf.")
        |> replace_file('CHECKSUM', "72749F733A9825E0DEAD3378AD7FB9AC97559A4E9D6BD1F73E8A6C349A662203")

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])
      assert {:error, {:metadata, {:user, 'illegal atom asdfasdfasdfasdf'}}} = Hex.Tar.unpack("badmetadata.tar", :memory)

      files =
        valid_files
        |> replace_file('metadata.config', "ok.")
        |> replace_file('CHECKSUM', "B4BF57D33731B5C4D64055BEF7BB0D4A80DB5ACD8A2117A6657A14DC5C207E2E")

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])
      assert {:error, {:metadata, :not_key_value}} = Hex.Tar.unpack("badmetadata.tar", :memory)

      # contents
      files =
        valid_files
        |> replace_file('contents.tar.gz', "badtar")
        |> replace_file('CHECKSUM', "4C68A4E04D1B7B29A7511B27EFCCB6117F8748A99C98E4397EEF0F076E1C19AB")

      :ok = :hex_erl_tar.create('badcontents.tar', files, [:write])
      assert {:error, {:inner_tarball, :eof}} = Hex.Tar.unpack("badcontents.tar", :memory)
    end)
  end

  defp replace_file(files, file, content) do
    List.keyreplace(files, file, 0, {file, content})
  end

  defp stringify(%{} = map) do
    Enum.into(map, %{}, &stringify/1)
  end
  defp stringify(list) when is_list(list) do
    Enum.map(list, &stringify/1)
  end
  defp stringify({key, value}) do
    {stringify(key), stringify(value)}
  end
  defp stringify(atom) when is_atom(atom) do
    Atom.to_string(atom)
  end
  defp stringify(true) do
    "true"
  end
  defp stringify(false) do
    "false"
  end
  defp stringify(value) do
    value
  end
end
