defmodule Hex.Resolver.BacktracksTest do
  use HexTest.Case
  import Hex.Resolver.Backtracks, only: [message: 1]

  setup do
    Hex.State.put(:offline?, true)
    Hex.Registry.open!(Hex.Registry.Server, registry_path: tmp_path("cache.ets"))
    Hex.Registry.prefetch(["foo"])
  end

  test "merge versions" do
    assert IO.iodata_to_binary(message({"foo", ["0.0.1", "0.1.0", "0.2.0"], []})) ==
           format([:underline, ~s'Failed to use "foo" (versions 0.0.1 to 0.2.0) because', :reset, "\n"])

    assert IO.iodata_to_binary(message({"foo", ["0.0.1", "0.1.0", "0.2.1"], []})) ==
           format([:underline, ~s'Failed to use "foo" (versions 0.0.1, 0.1.0, 0.2.1) because', :reset, "\n"])

    assert IO.iodata_to_binary(message({"foo", ["0.1.0", "0.2.1"], []})) ==
           format([:underline, ~s'Failed to use "foo" (versions 0.1.0 and 0.2.1) because', :reset, "\n"])
  end

  defp format(message) do
    message
    |> IO.ANSI.format
    |> IO.iodata_to_binary
  end
end
