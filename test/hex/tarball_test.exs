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

  test "create rejects escaping symlinks and accepts internal symlinks" do
    metadata = %{name: "foo", version: "1.0.0"}

    in_tmp(fn ->
      File.write!("README.md", "README")
      File.mkdir!("dir")
      File.ln_s!("../README.md", "dir/internal")
      File.ln_s!("../../README.md", "dir/escaping")

      assert {:ok, _} =
               :mix_hex_tarball.create(metadata, [
                 {~c"README.md", "README"},
                 {~c"dir/internal", ~c"dir/internal"}
               ])

      assert {:error, {:tarball, {:unsafe_symlink, ~c"dir/escaping", ~c"../../README.md"}}} =
               :mix_hex_tarball.create(metadata, [{~c"dir/escaping", ~c"dir/escaping"}])

      assert {:ok, _} =
               :mix_hex_tarball.create_docs([
                 {~c"README.md", "README"},
                 {~c"dir/internal", ~c"dir/internal"}
               ])

      assert {:error, {:tarball, {:unsafe_symlink, ~c"dir/escaping", ~c"../../README.md"}}} =
               :mix_hex_tarball.create_docs([{~c"dir/escaping", ~c"dir/escaping"}])
    end)
  end

  test "create rejects unsupported file types" do
    if mkfifo = System.find_executable("mkfifo") do
      metadata = %{name: "foo", version: "1.0.0"}

      in_tmp(fn ->
        System.cmd(mkfifo, ["fifo"])

        assert {:error, {:tarball, {:unsupported_file_type, ~c"fifo", :other}}} =
                 :mix_hex_tarball.create(metadata, [{~c"fifo", ~c"fifo"}])

        assert {:error, {:tarball, {:unsupported_file_type, ~c"fifo", :other}}} =
                 :mix_hex_tarball.create_docs([{~c"fifo", ~c"fifo"}])
      end)
    end
  end
end
