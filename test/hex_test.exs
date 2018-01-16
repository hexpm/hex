defmodule HexDemoTest do
  use HexTest.Case
  @moduletag :integration

  test "unpack and verify tar" do
    in_tmp(fn ->
      repo = "hexpm"
      name = "ex_doc"
      version = "0.1.0"

      case Hex.Repo.get_tarball(repo, name, version, nil) do
        {:ok, {200, body, _headers}} ->
          parent_directory = File.cwd!()
          path = Path.join(parent_directory, "#{name}-#{version}.tar")
          File.write!(path, body)

          {meta, _checksum} = Hex.unpack_and_verify_tar!(path, parent_directory, repo, name, version)

          assert "ex_doc" == Map.get(meta, "app")
        {:ok, {code, _body, _headers}} ->
          Mix.raise("Request failed (#{code})")
        {:error, reason} ->
          Mix.raise("Request failed (#{inspect reason})")
      end
    end)
  end

  test "adapt organization to repo" do
    opts = [organization: "acme"]
    opts = Hex.organization_to_repo(opts)
    refute Keyword.has_key?(opts, :organization)
    assert "hexpm:acme" == Keyword.get(opts, :repo)
  end
end
