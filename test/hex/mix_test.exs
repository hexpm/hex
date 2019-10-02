defmodule Hex.MixTest do
  use HexTest.Case

  test "from_lock/1" do
    lock = [ex_doc: {:hex, :ex_doc, "0.1.0"}, postgrex: {:hex, :fork, "0.2.1"}]

    assert Hex.Mix.from_lock(lock) ==
             [{"hexpm", "ex_doc", "ex_doc", "0.1.0"}, {"hexpm", "fork", "postgrex", "0.2.1"}]
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

  test "flatten_deps/2 with only dependencies" do
    ecto = %Mix.Dep{app: :ecto, deps: [], top_level: false}
    postgrex = %Mix.Dep{app: :postgrex, deps: [], top_level: false}
    ex_doc = %Mix.Dep{app: :ex_doc, deps: [], top_level: false, opts: [only: :doc]}
    phoenix = %Mix.Dep{app: :phoenix, deps: [ecto, postgrex, ex_doc], top_level: true}

    deps = [phoenix, ecto, postgrex]

    flattened_deps = Hex.Mix.flatten_deps(deps, [:phoenix])
    assert phoenix in flattened_deps
    assert ecto in flattened_deps
    assert postgrex in flattened_deps
    refute ex_doc in flattened_deps
  end

  test "flatten_deps/2 with overridden dependencies" do
    ecto = %Mix.Dep{app: :ecto, deps: [], top_level: false}
    postgrex = %Mix.Dep{app: :postgrex, deps: [], top_level: false, opts: [override: true]}
    overridden_postgrex = %Mix.Dep{app: :postgrex, deps: [], top_level: false}
    phoenix = %Mix.Dep{app: :phoenix, deps: [ecto, overridden_postgrex], top_level: true}

    deps = [ecto, postgrex, phoenix]

    flattened_deps = Hex.Mix.flatten_deps(deps, [:phoenix, :postgrex])
    assert phoenix in flattened_deps
    assert ecto in flattened_deps
    assert postgrex in flattened_deps
    refute overridden_postgrex in flattened_deps
  end
end
