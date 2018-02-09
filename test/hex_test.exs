defmodule HexTest do
  use HexTest.Case

  test "adapt organization to repo" do
    opts = [organization: "acme"]
    opts = Hex.organization_to_repo(opts)
    refute Keyword.has_key?(opts, :organization)
    assert "hexpm:acme" == Keyword.get(opts, :repo)
  end
end
