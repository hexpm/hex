defmodule Hex.RepoTest do
  use HexTest.Case
  @moduletag :integration

  test "request" do
    bypass_mirror()

    path = "docs/package-1.1.2.tar.gz"
    package_url = Hex.API.repo_url(path)
    bad_url = Hex.API.repo_url("docs/package")

    assert {:ok, _, nil} = Hex.Repo.request(package_url, nil)
    assert {:ok, _, nil} = Hex.Repo.request(package_url, 'etag')
    assert {:error, "Request failed (404)"} = Hex.Repo.request(bad_url, nil)
  end
end
