defmodule Hex.TarTest do
  use HexTest.Case
  @moduletag :integration

  test "unpack should include hex_metadata.config in deps" do
    Mix.Project.push(ReleaseIncludeRepoDeps.MixProject)

    in_tmp(fn ->
      package = "ex_doc"
      path = "deps/#{package}/hex_metadata.config"
      Hex.State.put(:home, tmp_path())
      Mix.Tasks.Deps.Get.run([])

      assert File.exists?(path)

      assert {:ok, meta} = :file.consult(path)
      meta = Enum.into(meta, %{})
      assert package == meta["app"]
      assert "0.0.1" == meta["version"]
    end)
  after
    purge([ReleaseIncludeRepoDeps.MixProject])
  end
end
