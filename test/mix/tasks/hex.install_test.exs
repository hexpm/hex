defmodule Mix.Tasks.Hex.InstallTest do
  use HexTest.Case
  @moduletag :integration

  test "install/1" do
    assert_raise Mix.Error, "package_name is not a valid Hex version", fn ->
      Mix.Tasks.Hex.Install.run(["package_name"])
    end
  end
end
