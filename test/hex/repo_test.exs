defmodule Hex.RepoTest do
  use HexTest.Case
  @moduletag :integration

  test "request" do
    bypass_mirror()

    repo_url = Hex.State.fetch!(:repos)["hexpm"].url
    package_url = repo_url <> "/docs/package-1.1.2.tar.gz"
    bad_url = repo_url <> "/docs/package"

    assert {:ok, _, nil} = Hex.Repo.request(package_url, nil)
    assert {:ok, _, nil} = Hex.Repo.request(package_url, "etag")
    assert {:error, "Request failed (404)"} = Hex.Repo.request(bad_url, nil)
  end
end
