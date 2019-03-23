defmodule Mix.Tasks.Hex.PackageTest do
  use HexTest.Case
  @moduletag :integration

  test "success" do
    bypass_package()
    package = "package"
    version = "1.0.0"
    path = tmp_path()
    Hex.State.put(:home, path)

    in_tmp("package", fn ->
      Mix.Tasks.Hex.Package.run(["fetch", package, version])
      msg = "#{package} v#{version} downloaded to #{path}/package/package-1.0.0.tar"
      assert_received {:mix_shell, :info, [^msg]}
      assert File.exists?("#{path}/package/package-1.0.0.tar")
    end)
  end

  test "error when http error" do
  end

  test "error when unknown error" do
  end

  test "error when request is timeouted" do
  end

  test "error when no argument" do
    error_msg = """
      Invalid arguments, expected one of:

      mix hex.package fetch PACKAGE VERSION [--unpack]
    """

    assert_raise Mix.Error, error_msg, fn ->
      Mix.Tasks.Hex.Package.run([])
    end
  end
end
