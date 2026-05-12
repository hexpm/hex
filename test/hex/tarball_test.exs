defmodule Hex.TarballTest do
  use HexTest.Case

  test "create rejects unsafe archive paths" do
    metadata = %{name: "foo", version: "1.0.0"}

    assert {:error, {:tarball, {:unsafe_path, ~c"../README.md"}}} =
             :mix_hex_tarball.create(metadata, [{~c"../README.md", "README"}])

    assert {:error, {:tarball, {:unsafe_path, ~c"/README.md"}}} =
             :mix_hex_tarball.create(metadata, [{~c"/README.md", "README"}])

    assert {:error, {:tarball, {:unsafe_path, ~c"../README.md"}}} =
             :mix_hex_tarball.create_docs([{~c"../README.md", "README"}])

    assert {:error, {:tarball, {:unsafe_path, ~c"/README.md"}}} =
             :mix_hex_tarball.create_docs([{~c"/README.md", "README"}])
  end

  test "create requires root-relative filesystem source paths" do
    metadata = %{name: "foo", version: "1.0.0"}

    in_tmp(fn ->
      File.write!("README.md", "README")

      root = File.cwd!() |> String.to_charlist()
      config = :mix_hex_core.default_config() |> Map.put(:tarball_files_root, root)
      absolute_readme = Path.expand("README.md") |> String.to_charlist()

      assert {:error, {:tarball, :missing_files_root}} =
               :mix_hex_tarball.create(metadata, [{~c"README.md", ~c"README.md"}])

      assert {:error, {:tarball, :missing_files_root}} =
               :mix_hex_tarball.create_docs([{~c"README.md", ~c"README.md"}])

      assert {:error, {:tarball, {:unsafe_path, ^absolute_readme}}} =
               :mix_hex_tarball.create(metadata, [{~c"README.md", absolute_readme}], config)

      assert {:error, {:tarball, {:unsafe_path, ^absolute_readme}}} =
               :mix_hex_tarball.create_docs([{~c"README.md", absolute_readme}], config)

      assert {:ok, _} =
               :mix_hex_tarball.create(metadata, [{~c"README.md", ~c"README.md"}], config)

      assert {:ok, _} =
               :mix_hex_tarball.create_docs([{~c"README.md", ~c"README.md"}], config)

      File.mkdir!("../outside")
      File.write!("../outside/secret.txt", "outside")
      File.ln_s!("../outside/secret.txt", "mismatch_link")

      assert {:error, {:tarball, {:unsafe_path, ~c"nested/mismatch_link"}}} =
               :mix_hex_tarball.create(
                 metadata,
                 [{~c"nested/mismatch_link", ~c"mismatch_link"}],
                 config
               )

      assert {:error, {:tarball, {:unsafe_path, ~c"nested/mismatch_link"}}} =
               :mix_hex_tarball.create_docs(
                 [{~c"nested/mismatch_link", ~c"mismatch_link"}],
                 config
               )
    end)
  end

  test "create rejects escaping symlinks and accepts internal symlinks" do
    metadata = %{name: "foo", version: "1.0.0"}

    in_tmp(fn ->
      File.write!("README.md", "README")
      File.mkdir!("dir")
      File.ln_s!("../README.md", "dir/internal")
      File.ln_s!("../../README.md", "dir/escaping")

      config =
        :mix_hex_core.default_config()
        |> Map.put(:tarball_files_root, File.cwd!() |> String.to_charlist())

      assert {:ok, _} =
               :mix_hex_tarball.create(
                 metadata,
                 [
                   {~c"README.md", "README"},
                   {~c"dir/internal", ~c"dir/internal"}
                 ],
                 config
               )

      assert {:error, {:tarball, {:unsafe_symlink, ~c"dir/escaping", ~c"../../README.md"}}} =
               :mix_hex_tarball.create(
                 metadata,
                 [{~c"dir/escaping", ~c"dir/escaping"}],
                 config
               )

      assert {:ok, _} =
               :mix_hex_tarball.create_docs(
                 [
                   {~c"README.md", "README"},
                   {~c"dir/internal", ~c"dir/internal"}
                 ],
                 config
               )

      assert {:error, {:tarball, {:unsafe_symlink, ~c"dir/escaping", ~c"../../README.md"}}} =
               :mix_hex_tarball.create_docs([{~c"dir/escaping", ~c"dir/escaping"}], config)
    end)
  end

  test "create rejects unsupported file types" do
    if mkfifo = System.find_executable("mkfifo") do
      metadata = %{name: "foo", version: "1.0.0"}

      in_tmp(fn ->
        System.cmd(mkfifo, ["fifo"])

        config =
          :mix_hex_core.default_config()
          |> Map.put(:tarball_files_root, File.cwd!() |> String.to_charlist())

        assert {:error, {:tarball, {:unsupported_file_type, ~c"fifo", :other}}} =
                 :mix_hex_tarball.create(metadata, [{~c"fifo", ~c"fifo"}], config)

        assert {:error, {:tarball, {:unsupported_file_type, ~c"fifo", :other}}} =
                 :mix_hex_tarball.create_docs([{~c"fifo", ~c"fifo"}], config)
      end)
    end
  end
end
