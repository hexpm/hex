defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  test "search" do
    Mix.Tasks.Hex.Search.run(["doc"])
    assert_received {:mix_shell, :info, ["ex_doc\e[0m" <> ex_doc]}
    assert_received {:mix_shell, :info, ["only_doc\e[0m" <> only_doc]}
    assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
    assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
  end

  test "empty search" do
    Mix.Tasks.Hex.Search.run(["bloopdoopbloop"])
    assert_received {:mix_shell, :info, ["No packages found"]}
  end

  # TODO: Right now we can't test this properly. Consider that we can't test
  # this new feature without a private package, and we can't test a private
  # packages without being able to create organizations first. The latter
  # would require some changes from on the hexpm side.
  test "search with authenticated user" do
    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")
      Mix.Tasks.Hex.Config.run(["username", "user"])

      send self(), {:mix_shell_input, :yes?, false}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Search.run(["doc"])

      assert_received {:mix_shell, :info, ["ex_doc\e[0m" <> ex_doc]}
      assert_received {:mix_shell, :info, ["only_doc\e[0m" <> only_doc]}
      assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
      assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
    end
  end
end
