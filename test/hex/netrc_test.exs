defmodule Hex.NetrcTest do
  use HexTest.Case

  alias Hex.Netrc

  setup do
    # Restart Hex between tests to get a fresh cache.
    Application.stop(:hex)
    :ok = Application.start(:hex)
  end

  test "lookup/2 returns nil for unknown host" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
        login foo
        password bar
      """

      File.write!(".netrc", data)

      assert {:ok, nil} = Netrc.lookup("bar.example.com", ".netrc")
    end)
  end

  test "lookup/2 returns expected result for known host" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
        login foo
        password bar
      """

      File.write!(".netrc", data)

      assert {:ok, %{username: "foo", password: "bar"}} =
               Netrc.lookup("foo.example.com", ".netrc")
    end)
  end
end
