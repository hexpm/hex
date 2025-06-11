defmodule Hex.UtilsTest do
  use ExUnit.Case

  setup do
    on_exit(fn -> Process.delete(:hex_repo_identifier) end)
  end

  describe "repo_identifier/0" do
    test "an identifier is included within a repository" do
      assert Hex.Utils.repo_identifier() =~ ~r/^[a-f0-9]{64}$/
    end

    test "identifier is nil outside of a repository" do
      # The tmp_dir resolves at hex/test/tmp, which allows git to traverse up to the repository
      # root and find a commit. We're creating a temporary directory to simulate being outside of
      # a repository instead.
      dir =
        "../../.."
        |> Path.expand(__DIR__)
        |> Path.join("empty-directory")

      try do
        File.mkdir!(dir)

        File.cd!(dir, fn -> refute Hex.Utils.repo_identifier() end)
      after
        File.rmdir(dir)
      end
    end

    test "identifier is nil when disabled by an environment variable" do
      System.put_env("HEX_REPO_IDENTIFIER", "0")
      refute Hex.Utils.repo_identifier()

      System.put_env("HEX_REPO_IDENTIFIER", "false")
      refute Hex.Utils.repo_identifier()
    after
      System.delete_env("HEX_REPO_IDENTIFIER")
    end

    test "the identifier is cached within the current process" do
      value = "cached-identifier"

      Process.put(:hex_repo_identifier, value)

      assert value == Hex.Utils.repo_identifier()
    end
  end
end
