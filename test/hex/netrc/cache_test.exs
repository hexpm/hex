defmodule Hex.Netrc.CacheTest do
  use HexTest.Case, async: false

  alias Hex.Netrc.Cache

  setup do
    # Restart Hex between tests to get a fresh cache.
    Application.stop(:hex)
    :ok = Application.start(:hex)
  end

  test "fetch/1 fails on non-existent file" do
    in_tmp(fn ->
      assert {:error, :enoent} = Cache.fetch(".netrc")
    end)
  end

  test "fetch/1 remembers parse errors" do
    in_tmp(fn ->
      File.write!(".netrc", "")
      assert {:error, :parse} = Cache.fetch(".netrc")
      File.rm!(".netrc")
      assert {:error, :parse} = Cache.fetch(".netrc")
    end)
  end

  test "fetch/1 succeeds on simple file" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
        login john
        password bar
      """

      parsed = %{
        "foo.example.com" => %{
          username: "john",
          password: "bar"
        }
      }

      File.write!(".netrc", data)
      assert {:ok, ^parsed} = Cache.fetch(".netrc")
    end)
  end

  test "fetch/1 remembers successful parses" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
        login john
        password bar
      """

      parsed = %{
        "foo.example.com" => %{
          username: "john",
          password: "bar"
        }
      }

      File.write!(".netrc", data)
      assert {:ok, ^parsed} = Cache.fetch(".netrc")
      File.rm!(".netrc")
      assert {:ok, ^parsed} = Cache.fetch(".netrc")
    end)
  end
end
