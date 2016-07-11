defmodule Mix.Tasks.HexTest do
  use HexTest.Case

  test "run without args shows help" do
    # Workaround for prune_deps in Elixir 1.4-dev
    paths = :code.get_path
    try do
      Mix.Tasks.Hex.run([])
      assert_received {:mix_shell, :info, ["Hex is a package manager for the Erlang ecosystem."]}
    after
      true = :code.set_path(paths)
    end
  end

  test "run with invalid arguments" do
    assert_raise Mix.Error, "Invalid arguments, expected: mix hex", fn ->
      Mix.Tasks.Hex.run(["invalid"])
    end
  end
end
