defmodule Hex.UtilsTest do
  use ExUnit.Case

  describe "client_identifier/0" do
    test "an identifier is included within a repository" do
      assert Hex.Utils.client_identifier() =~ ~r/^[a-f0-9]{64}$/
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

        File.cd!(dir, fn -> refute Hex.Utils.client_identifier() end)
      after
        File.rmdir(dir)
      end
    end
  end
end
