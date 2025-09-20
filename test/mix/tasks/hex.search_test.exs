defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.IntegrationCase

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
      set_home_tmp()
      auth = Hexpm.new_user("searchuser1", "searchuser1@mail.com", "password", "searchuser1")
      Hexpm.new_repo("searchrepo1", auth)
      Mix.Tasks.Hex.update_keys(auth[:key], auth[:key])

      Mix.Tasks.Hex.Search.run(["doc"])

      assert_received {:mix_shell, :info, ["ex_doc" <> ex_doc]}
      assert_received {:mix_shell, :info, ["only_doc" <> only_doc]}
      assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
      assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
    end)
  end

  test "search private package" do
    in_tmp(fn ->
      set_home_tmp()
      auth = Hexpm.new_user("searchuser2", "searchuser2@mail.com", "password", "searchuser2")
      Hexpm.new_repo("searchrepo2", auth)
      Mix.Tasks.Hex.update_keys(auth[:key], auth[:key])

      Mix.Tasks.Hex.Search.run(["doc", "--organization", "searchrepo2"])

      refute_received {:mix_shell, :info, ["ex_doc" <> _]}
      refute_received {:mix_shell, :info, ["only_doc" <> _]}
    end)
  end
end
