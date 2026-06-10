defmodule Hex.PolicyTest do
  use HexTest.Case, async: true
  alias Hex.Policy

  describe "parse_config/1" do
    test "accepts a keyword list (mix.exs form)" do
      assert {:ok, {"myorg", "strict-prod"}} ==
               Policy.parse_config(repo: "myorg", name: "strict-prod")
    end

    test "accepts a repo/name string (env-var form)" do
      assert {:ok, {"myorg", "strict-prod"}} == Policy.parse_config("myorg/strict-prod")
    end

    test "trims whitespace in the string form" do
      assert {:ok, {"myorg", "strict-prod"}} == Policy.parse_config("  myorg/strict-prod  ")
    end

    test "treats nil and empty as no policy" do
      assert {:ok, nil} == Policy.parse_config(nil)
      assert {:ok, nil} == Policy.parse_config("")
      assert {:ok, nil} == Policy.parse_config([])
    end

    test "rejects malformed entries" do
      assert :error == Policy.parse_config("missing-slash")
      assert :error == Policy.parse_config("a/b/c")
      assert :error == Policy.parse_config("myorg/strict,acme/baseline")
      assert :error == Policy.parse_config(repo: "x")
      assert :error == Policy.parse_config(42)
    end

    test "rejects the bare hexpm repo (no organization scope)" do
      assert :error == Policy.parse_config("hexpm/strict")
      assert :error == Policy.parse_config(repo: "hexpm", name: "strict")
    end
  end
end
