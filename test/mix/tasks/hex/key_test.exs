defmodule Mix.Tasks.Hex.KeyTest do
  use HexTest.Case

  test "mix hex.key task is deprecated" do
    deprecation_msg = """
    [deprecation] The mix hex.key task is deprecated, please use:
      mix hex.user
    """

    assert_raise Mix.Error, deprecation_msg, fn ->
      Mix.Tasks.Hex.Key.run([""])
    end
  end
end
