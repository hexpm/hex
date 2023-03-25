defmodule Hex.MixTest do
  use HexTest.Case

  test "from_lock/1" do
    lock = [ex_doc: {:hex, :ex_doc, "0.1.0"}, postgrex: {:hex, :fork, "0.2.1"}]

    assert Hex.Mix.from_lock(lock) ==
             [
               %{repo: "hexpm", name: "ex_doc", app: "ex_doc", version: "0.1.0"},
               %{repo: "hexpm", name: "fork", app: "postgrex", version: "0.2.1"}
             ]
  end

  test "from_lock/1 warns on newer lock versions" do
    message =
      {:mix_shell, :info,
       [
         "\e[33mThe mix.lock file was generated with a newer version of Hex. " <>
           "Update your client by running `mix local.hex` to avoid losing data.\e[0m"
       ]}

    lock = [
      ex_doc:
        {:hex, :ex_doc, "0.1.0", "checksum", [:mix], [{:dep, ">= 0.0.0", [hex: :dep]}], "hexpm",
         "checksum"}
    ]

    Hex.Server.reset()
    Hex.Mix.from_lock(lock)
    refute_received ^message

    lock = [
      ex_doc:
        {:hex, :ex_doc, "0.1.0", "checksum", [:mix], [{:dep, ">= 0.0.0", [hex: :dep]}], "hexpm",
         "checksum", "entry from newer version"}
    ]

    Hex.Server.reset()
    Hex.Mix.from_lock(lock)
    assert_received ^message

    Hex.Mix.from_lock(lock)
    refute_received ^message
  end
end
