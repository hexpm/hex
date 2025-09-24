defmodule Hex.RepoIdentifierTest do
  use ExUnit.Case

  alias Hex.RepoIdentifier

  setup do
    RepoIdentifier.clear()

    :ok
  end

  describe "get/0" do
    test "an identifier is included within a repository" do
      RepoIdentifier.clear()
      assert RepoIdentifier.get() =~ ~r/^[a-f0-9]{64}$/
    end

    test "identifier is nil outside of a repository" do
      dir = Path.join(System.tmp_dir!(), Base.encode16(:crypto.strong_rand_bytes(8)))
      File.mkdir!(dir)
      File.cd!(dir, fn -> refute RepoIdentifier.get() end)
    end

    test "identifier is nil when disabled by an environment variable" do
      System.put_env("HEX_NO_REPO_IDENTIFIER", "1")
      Hex.State.refresh()

      refute RepoIdentifier.get()
    after
      System.delete_env("HEX_NO_REPO_IDENTIFIER")
      Hex.State.refresh()
    end
  end

  describe "fetch/0" do
    test "the identifier is cached accross calls" do
      value = "cached-identifier"
      RepoIdentifier.put(value)

      assert value == RepoIdentifier.fetch()
    end
  end
end
