defmodule Hex.RepoTest do
  use HexTest.Case
  @moduletag :integration

  test "get_package" do
    assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")
  end
end
