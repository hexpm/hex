defmodule Mix.Tasks.Hex.RegistryTest do
  use HexTest.Case

  alias Mix.Tasks.Hex.Registry

  test "dump" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      File.write!("registry.ets.gz", "ETS")
      dest = Path.expand("dest.ets.gz")
      Registry.run(["dump", dest])
      assert File.read!(dest) == "ETS"
    end
  end

  test "load" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      source = Path.expand("source.ets.gz")
      File.write!(source, :zlib.gzip("ETS"))
      Registry.run(["load", source])
      path = Hex.Registry.path
      path_gz = path <> ".gz"
      assert File.read!(path) == "ETS"
      assert File.regular?(path_gz)
    end
  end
end
