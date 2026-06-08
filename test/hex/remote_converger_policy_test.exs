defmodule Hex.RemoteConvergerPolicyTest do
  use HexTest.Case

  test "with no policies configured, Hex.Policy.load_all returns empty" do
    in_tmp("remote_converger_no_policy", fn ->
      Hex.State.put(:config_home, File.cwd!())
      original = System.get_env("HEX_POLICY")
      System.delete_env("HEX_POLICY")

      try do
        Hex.State.refresh()
        assert {:ok, %{}} = Hex.Policy.load_all()
      after
        case original do
          nil -> System.delete_env("HEX_POLICY")
          value -> System.put_env("HEX_POLICY", value)
        end
      end
    end)
  end
end
