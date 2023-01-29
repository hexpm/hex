defmodule Mix.Tasks.Hex.InstallTest do
  use HexTest.IntegrationCase

  test "install/1" do
    assert_raise Mix.Error, fn ->
      Mix.Tasks.Hex.Install.run(["package_name"])
    end
  end
end
