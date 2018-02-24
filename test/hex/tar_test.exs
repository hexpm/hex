defmodule Hex.TarTest do
  use HexTest.Case

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
        app: :bar,
        optional: "false",
        requirement: "0.1.0"
      }
    },
    files: ["mix.exs"],
    build_tools: ["mix"]
  }

  @files ["mix.exs"]

  unix_epoch = :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
  y2k = :calendar.datetime_to_gregorian_seconds({{2000, 1, 1}, {0, 0, 0}})
  @epoch y2k - unix_epoch

  test "create and unpack on disk" do
    in_tmp(fn ->
      File.write!("mix.exs", @mix_exs)

      assert {:ok, {tar, checksum}} = Hex.Tar.create(@metadata, @files, "a/b.tar")
      assert {:ok, {metadata, ^checksum}} = Hex.Tar.unpack("a/b.tar", "unpack/")

      {:ok, [version_entry, _, _, _]} = :hex_erl_tar.table('a/b.tar', [:verbose])
      assert {'VERSION', :regular, _, @epoch, 0o100644, 0, 0} = version_entry

      assert byte_size(checksum) == 32
      assert File.read!("a/b.tar") == tar
      assert Enum.sort(File.ls!("unpack")) == ["hex_metadata.config", "mix.exs"]
      assert {:ok, metadata_kw} = :file.consult("unpack/hex_metadata.config")

      assert :proplists.get_keys(metadata_kw) ==
               ~w(files name app build_tools version requirements)

      assert File.read!("unpack/mix.exs") == File.read!("mix.exs")
      assert metadata == stringify(@metadata)
      assert metadata["build_tools"] == ["mix"]
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

  test "unpack with legacy requirements format" do
    in_tmp(fn ->
      files = [{"mix.exs", @mix_exs}]
      {:ok, {tar, _checksum}} = Hex.Tar.create(@metadata, files, :memory)
      {:ok, files} = :hex_erl_tar.extract({:binary, tar}, [:memory])

      files =
        files
        |> replace_file(
          'CHECKSUM',
          "351362112A71B8A6F4926E83E9DA37A87A8436A11B7718F2B168827E8B484607"
        )
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

      assert {:ok, {metadata, _checksum, _files}} =
               Hex.Tar.unpack("legacy_requirements.tar", :memory)

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

      assert {:error, {:tarball, {"missing.tar", :enoent}}} =
               Hex.Tar.unpack("missing.tar", :memory)

      :ok = :hex_erl_tar.create('empty.tar', [], [:write])
      assert {:error, {:tarball, :empty}} = Hex.Tar.unpack("empty.tar", :memory)

      :ok = :hex_erl_tar.create('missing.tar', [{'VERSION', "3"}], [:write])

      assert {:error, {:tarball, {:missing_files, ['CHECKSUM' | _]}}} =
               Hex.Tar.unpack("missing.tar", :memory)

      files = [
        {'VERSION', ""},
        {'CHECKSUM', ""},
        {'metadata.config', ""},
        {'contents.tar.gz', ""},
        {'invalid.txt', ""}
      ]

      :ok = :hex_erl_tar.create('missing.tar', files, [:write])

      assert {:error, {:tarball, {:invalid_files, ['invalid.txt']}}} =
               Hex.Tar.unpack("missing.tar", :memory)

      files = replace_file(valid_files, 'VERSION', "0")
      :ok = :hex_erl_tar.create('badversion.tar', files, [:write])
      assert {:error, {:tarball, {:bad_version, "0"}}} = Hex.Tar.unpack("badversion.tar", :memory)

      # checksum
      files = replace_file(valid_files, 'CHECKSUM', "bad")
      :ok = :hex_erl_tar.create('badchecksum.tar', files, [:write])
      assert {:error, :invalid_checksum} = Hex.Tar.unpack("badchecksum.tar", :memory)

      files = replace_file(valid_files, 'metadata.config', "{<<\"name\">>,<<\"foo\">>}")
      :ok = :hex_erl_tar.create('checksummismatch.tar', files, [:write])

      assert {:error, {:checksum_mismatch, _, _}} =
               Hex.Tar.unpack("checksummismatch.tar", :memory)

      # metadata
      files =
        valid_files
        |> replace_file('metadata.config', "ok $")
        |> replace_file(
          'CHECKSUM',
          "693703C2207D202A3E576A3D1F40DA58D7250C374F61950964B3CE54EB8B82F3"
        )

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])
      assert {:error, {:metadata, {:illegal, '$'}}} = Hex.Tar.unpack("badmetadata.tar", :memory)

      files =
        valid_files
        |> replace_file('metadata.config', "ok[")
        |> replace_file(
          'CHECKSUM',
          "E4923763C9CB95EDEFF724A2315468908DB05CD898D4C06FBA95286EB0F7F2C1"
        )

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])
      assert {:error, {:metadata, :invalid_terms}} = Hex.Tar.unpack("badmetadata.tar", :memory)

      files =
        valid_files
        |> replace_file('metadata.config', "asdfasdfasdfasdf.")
        |> replace_file(
          'CHECKSUM',
          "8A877C0E897795F243038FD4DF14A89D1E061E8E4CB6205F5EFF93CCB784E49B"
        )

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])

      assert {:error, {:metadata, {:user, 'illegal atom asdfasdfasdfasdf'}}} =
               Hex.Tar.unpack("badmetadata.tar", :memory)

      files =
        valid_files
        |> replace_file('metadata.config', "ok.")
        |> replace_file(
          'CHECKSUM',
          "3B2C506744F484D417440D66C680B554844B5FE2574E627E77778877F21D69AF"
        )

      :ok = :hex_erl_tar.create('badmetadata.tar', files, [:write])
      assert {:error, {:metadata, :not_key_value}} = Hex.Tar.unpack("badmetadata.tar", :memory)

      # contents
      files =
        valid_files
        |> replace_file('contents.tar.gz', "badtar")
        |> replace_file(
          'CHECKSUM',
          "892343F3E69A7408B75BFF02F438716B890828E06715EFED75F6FCAD2FC9E44E"
        )

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
