defmodule Hex.Resolver.BacktracksTest do
  use HexTest.Case
  import Hex.Resolver.Backtracks, only: [message: 1]
  alias Hex.Registry.Server, as: Registry

  setup do
    Hex.State.put(:offline?, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    Registry.prefetch([{"hexpm", "foo"}])
  end

  test "merge versions" do
    assert IO.iodata_to_binary(message({"foo", ["0.0.1", "0.1.0", "0.2.0"], "hexpm", []})) ==
           format([:underline, ~s'Failed to use "foo" (versions 0.0.1 to 0.2.0) because', :reset, "\n"])

    assert IO.iodata_to_binary(message({"foo", ["0.0.1", "0.1.0", "0.2.1"], "hexpm", []})) ==
           format([:underline, ~s'Failed to use "foo" (versions 0.0.1, 0.1.0, 0.2.1) because', :reset, "\n"])

    assert IO.iodata_to_binary(message({"foo", ["0.1.0", "0.2.1"], "hexpm", []})) ==
           format([:underline, ~s'Failed to use "foo" (versions 0.1.0 and 0.2.1) because', :reset, "\n"])
  end

  defp format(message) do
    message
    |> IO.ANSI.format
    |> IO.iodata_to_binary
  end
end
