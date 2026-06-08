defmodule Hex.Policy.SourcesTest do
  use HexTest.Case, async: false
  alias Hex.Policy.Sources

  describe "parse_config/1" do
    test "accepts a single keyword list" do
      assert {:ok, [{"myorg", "strict-prod"}]} ==
               Sources.parse_config(repo: "myorg", name: "strict-prod")
    end

    test "accepts a list of keyword lists" do
      assert {:ok, [{"acme", "a"}, {"acme", "b"}]} ==
               Sources.parse_config([
                 [repo: "acme", name: "a"],
                 [repo: "acme", name: "b"]
               ])
    end

    test "accepts a comma-separated string (env-var form)" do
      assert {:ok, [{"myorg", "strict-prod"}]} == Sources.parse_config("myorg/strict-prod")

      assert {:ok, [{"myorg", "strict-prod"}, {"acme", "baseline"}]} ==
               Sources.parse_config("myorg/strict-prod,acme/baseline")
    end

    test "treats nil and empty as no policies" do
      assert {:ok, []} == Sources.parse_config(nil)
      assert {:ok, []} == Sources.parse_config("")
      assert {:ok, []} == Sources.parse_config([])
    end

    test "rejects malformed entries" do
      assert :error == Sources.parse_config("missing-slash")
      assert :error == Sources.parse_config("a/b/c")
      assert :error == Sources.parse_config(repo: "x")
      assert :error == Sources.parse_config(42)
    end

    test "rejects the bare hexpm repo (no organization scope)" do
      assert :error == Sources.parse_config("hexpm/strict")
      assert :error == Sources.parse_config(repo: "hexpm", name: "strict")
      assert :error == Sources.parse_config([[repo: "hexpm", name: "strict"]])
    end

    test "trims whitespace in env-var form" do
      assert {:ok, [{"myorg", "p"}, {"acme", "b"}]} ==
               Sources.parse_config(" myorg/p , acme/b ")
    end
  end

  describe "dedup/1" do
    test "dedups by (repo, name) preserving first-seen order" do
      assert [{"acme", "a"}, {"myorg", "b"}] ==
               Sources.dedup([{"acme", "a"}, {"myorg", "b"}, {"acme", "a"}])
    end

    test "is a no-op on already-unique input" do
      assert [{"acme", "a"}, {"acme", "b"}] == Sources.dedup([{"acme", "a"}, {"acme", "b"}])
    end
  end

  describe "load_all/0" do
    test "returns the empty set when no source contributes" do
      Hex.State.put(:policy_env, [])
      Hex.State.put(:policy_project, [])
      Hex.State.put(:policy_global, [])
      assert {:ok, []} == Sources.load_all()
    end

    test "unions and dedups refs across all three sources" do
      # Env comes first; project next; global last. Dedup keeps first-seen.
      Hex.State.put(:policy_env, [{"myorg", "strict"}, {"acme", "extra"}])
      Hex.State.put(:policy_project, [{"acme", "extra"}])
      Hex.State.put(:policy_global, [{"acme", "baseline"}, {"myorg", "strict"}])

      assert {:ok,
              [
                {"myorg", "strict"},
                {"acme", "extra"},
                {"acme", "baseline"}
              ]} == Sources.load_all()
    end
  end
end
