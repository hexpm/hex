defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  test "search" do
    Mix.Tasks.Hex.Search.run(["doc"])
    assert_received {:mix_shell, :info, ["ex_doc" <> ex_doc]}
    assert_received {:mix_shell, :info, ["only_doc" <> only_doc]}
    assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
    assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
  end

  test "empty search" do
    Mix.Tasks.Hex.Search.run(["bloopdoopbloop"])
    assert_received {:mix_shell, :info, ["No packages found"]}
  end

  test "search all private packages" do
    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      auth = Hexpm.new_user("search1", "search1@mail.com", "password", "search1")
      Hexpm.new_repo("search1", auth)
      Mix.Tasks.Hex.update_key(auth[:"$encrypted_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Search.run(["doc", "--all-organizations"])

      assert_received {:mix_shell, :info, ["ex_doc" <> ex_doc]}
      assert_received {:mix_shell, :info, ["only_doc" <> only_doc]}
      assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
      assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
    end)
  end

  @tag :skip
  # This test is good, but there is a bug in the search code it is testing.
  test "search private package" do
    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      auth = Hexpm.new_user("search2", "search2@mail.com", "password", "search2")
      Hexpm.new_repo("search2", auth)
      Mix.Tasks.Hex.update_key(auth[:"$encrypted_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Search.run(["doc", "--organization", "search2"])

      refute_received {:mix_shell, :info, ["ex_doc" <> _]}
      refute_received {:mix_shell, :info, ["only_doc" <> _]}
    end)
  end
end
