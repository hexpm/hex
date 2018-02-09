defmodule HexTest do
  use HexTest.Case

  test "organization_to_repo/1" do
    opts = Hex.organization_to_repo([organization: "acme"])
    assert Keyword.fetch!(opts, :repo) == "hexpm:acme"
  end
end
