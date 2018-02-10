defmodule Mix.Tasks.Hex.FetchTest do
  use HexTest.Case
  @moduletag :integration

  test "fetch latest version" do
    in_tmp(fn ->
      Mix.Tasks.Hex.Fetch.run(["ex_doc"])
      message = "Package fetched at: ex_doc-0.1.0.tar"
      assert_received {:mix_shell, :info, [^message]}
      assert File.exists?("ex_doc-0.1.0.tar")

      assert_raise Mix.Error, "No package with name no_package", fn ->
        Mix.Tasks.Hex.Fetch.run(["no_package"])
      end
    end)
  end

  test "package name is required" do
    message =
      "Invalid arguments, expected:\n\nmix hex.fetch PACKAGE [VERSION] [--output PATH] [--unpack]\n"

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Hex.Fetch.run([])
    end
  end

  test "fetch specific version of a package" do
    in_tmp(fn ->
      Mix.Tasks.Hex.Fetch.run(["ex_doc", "0.0.1"])
      message = "Package fetched at: ex_doc-0.0.1.tar"
      assert_received {:mix_shell, :info, [^message]}
      assert File.exists?("ex_doc-0.0.1.tar")
    end)
  end

  test "fetch to custom location" do
    in_tmp(fn ->
      output = "packages/ex_doc.tar"
      Mix.Tasks.Hex.Fetch.run(["ex_doc", "-o", output])
      message = "Package fetched at: #{output}"
      assert_received {:mix_shell, :info, [^message]}
      assert File.exists?(output)
    end)
  end

  test "fetch and unpack" do
    in_tmp(fn ->
      Mix.Tasks.Hex.Fetch.run(["ex_doc", "--unpack"])
      message = "Package fetched and unpacked to: ex_doc-0.1.0"
      assert_received {:mix_shell, :info, [^message]}
      refute File.exists?("ex_doc-0.1.0.tar")
      assert File.exists?("ex_doc-0.1.0/hex_metadata.config")
    end)
  end

  test "fetch and unpack to custom location" do
    in_tmp(fn ->
      output = "vendor/hex/ex_doc"
      Mix.Tasks.Hex.Fetch.run(["ex_doc", "--unpack", "-o", output])
      message = "Package fetched and unpacked to: #{output}"
      assert_received {:mix_shell, :info, [^message]}
      refute File.exists?("ex_doc-0.1.0.tar")
      assert File.exists?("#{output}/hex_metadata.config")
    end)
  end
end
