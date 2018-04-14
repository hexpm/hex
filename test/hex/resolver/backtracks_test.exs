defmodule Hex.Resolver.BacktracksTest do
  use HexTest.Case
  import Hex.Resolver.Backtracks, only: [message: 2]
  alias Hex.Registry.Server, as: Registry

  setup do
    Hex.State.put(:offline?, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    Registry.prefetch([{"hexpm", "foo"}])
  end

  test "merge versions" do
    expected =
      format([
        :underline,
        ~s'Failed to use "foo" (versions 0.0.1 to 0.2.0) ',
        "because there are no packages that matches the requirement",
        :reset,
        "\n"
      ])

    actual = message({"foo", ["0.0.1", "0.1.0", "0.2.0"], "hexpm", []}, Registry)
    assert expected == IO.iodata_to_binary(actual)

    expected =
      format([
        :underline,
        ~s'Failed to use "foo" (versions 0.0.1, 0.1.0, 0.2.1) ',
        "because there are no packages that matches the requirement",
        :reset,
        "\n"
      ])

    actual = message({"foo", ["0.0.1", "0.1.0", "0.2.1"], "hexpm", []}, Registry)
    assert expected == IO.iodata_to_binary(actual)

    expected =
      format([
        :underline,
        ~s'Failed to use "foo" (versions 0.1.0 and 0.2.1) ',
        "because there are no packages that matches the requirement",
        :reset,
        "\n"
      ])

    assert actual = message({"foo", ["0.1.0", "0.2.1"], "hexpm", []}, Registry)

    assert expected == IO.iodata_to_binary(actual)
  end

  defp format(message) do
    message
    |> Hex.Shell.format(Hex.Shell.ansi_enabled?())
    |> IO.iodata_to_binary()
  end
end
