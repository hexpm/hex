defmodule Hex.MixTest do
  use HexTest.Case

  defmodule Simple do
    def project do
      [ app: :simple,
        version: "0.1.0",
        deps: [ { :ecto, [package: true] } ] ]
    end
  end

  defmodule Override do
    def project do
      [ app: :override,
        version: "0.1.0",
        deps: [ { :ecto, package: true },
                { :ex_doc, "~> 0.1.0", package: true, override: true }] ]
    end
  end

  setup do
    Hex.Registry.start(registry_path: tmp_path("hex.ets"))
    :application.set_env(:hex, :registry_updated, false)
  end

  @tag :integration
  test "simple" do
    Mix.Project.push Simple

    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)
      Mix.Task.run "deps.get"

      assert_received { :mix_shell, :info, ["* Getting ecto (package)"] }
      assert_received { :mix_shell, :info, ["* Getting postgrex (package)"] }
      assert_received { :mix_shell, :info, ["* Getting ex_doc (package)"] }

      Mix.Task.run "deps.compile"
      Mix.Task.run "deps"

      assert_received { :mix_shell, :info, ["* ecto 0.2.0 (package)"] }
      assert_received { :mix_shell, :info, ["  locked at 0.2.0"] }
      assert_received { :mix_shell, :info, ["  ok"] }

      assert_received { :mix_shell, :info, ["* postgrex 0.2.0 (package)"] }
      assert_received { :mix_shell, :info, ["  locked at 0.2.0"] }
      assert_received { :mix_shell, :info, ["  ok"] }

      assert_received { :mix_shell, :info, ["* ex_doc 0.0.1 (package)"] }
      assert_received { :mix_shell, :info, ["  locked at 0.0.1"] }
      assert_received { :mix_shell, :info, ["  ok"] }
    end
  after
    purge [ Ecto.NoConflict.Mixfile, Postgrex.NoConflict.Mixfile,
            Ex_doc.NoConflict.Mixfile ]
    System.delete_env("MIX_HOME")
  end

  @tag :integration
  test "override" do
    Mix.Project.push Override

    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)

      Mix.Task.run "deps.get"
      Mix.Task.run "deps.compile"
      Mix.Task.run "deps"

      assert_received { :mix_shell, :info, ["* ecto 0.2.1 (package)"] }
      assert_received { :mix_shell, :info, ["  locked at 0.2.1"] }
      assert_received { :mix_shell, :info, ["  ok"] }

      assert_received { :mix_shell, :info, ["* postgrex 0.2.1 (package)"] }
      assert_received { :mix_shell, :info, ["  locked at 0.2.1"] }
      assert_received { :mix_shell, :info, ["  ok"] }

      assert_received { :mix_shell, :info, ["* ex_doc 0.1.0 (package)"] }
      assert_received { :mix_shell, :info, ["  locked at 0.1.0"] }
      assert_received { :mix_shell, :info, ["  ok"] }
    end
  after
    purge [ Ecto.NoConflict.Mixfile, Postgrex.NoConflict.Mixfile,
            Ex_doc.NoConflict.Mixfile ]
    System.delete_env("MIX_HOME")
  end

  test "config" do
    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)
      assert Hex.Mix.read_config == []

      Hex.Mix.update_config([key: "value"])
      assert Hex.Mix.read_config == [key: "value"]

      Hex.Mix.update_config([key: "other", foo: :bar])
      assert Hex.Mix.read_config == [key: "other", foo: :bar]
    end
  after
    System.delete_env("MIX_HOME")
  end

  test "from mixlock" do
    lock = [ ex_doc: { :package, "0.1.0" },
             postgrex: { :package, "0.2.1" } ]
    assert Hex.Mix.from_lock(lock) == [{ "ex_doc", "0.1.0" }, { "postgrex", "0.2.1" }]
  end
end
