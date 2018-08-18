defmodule Mix.Tasks.HexTest do
  use HexTest.Case

  test "run without args shows help" do
    Mix.Tasks.Hex.run([])
    assert_received {:mix_shell, :info, ["Hex is a package manager for the Erlang ecosystem."]}
  end
end
