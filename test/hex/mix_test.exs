defmodule Hex.MixTest do
  use HexTest.Case

  defmodule Foo do
    def project do
      [ app: :foo,
        version: "0.1.0",
        deps: [ { :ecto, [package: true] } ] ]
    end
  end

  setup do
    Hex.Registry.start(registry_path: tmp_path("hex.ets"))
  end

  test "simple" do
    Mix.Project.push Foo

    in_tmp fn _ ->
      Mix.Task.run "deps.get"

      assert_received { :mix_shell, :info, ["* Getting ecto (package)"] }
      assert_received { :mix_shell, :info, ["* Getting git_repo" <> _] }
      assert_received { :mix_shell, :info, ["* Getting postgrex (package)"] }
      assert_received { :mix_shell, :info, ["* Getting ex_doc (package)"] }

      Mix.Task.run "deps.compile"
      Mix.Task.run "deps"

      assert_received { :mix_shell, :info, ["* ecto 0.2.0 (package)"] }
      assert_received { :mix_shell, :info, ["* git_repo 0.1.0" <> _] }
      assert_received { :mix_shell, :info, ["* postgrex 0.2.0 (package)"] }
      assert_received { :mix_shell, :info, ["* ex_doc 0.0.1 (package)"] }
    end
  after
    purge [ Ecto.NoConflict.Mixfile, Git_repo.NoConflict.Mixfile,
            Postgrex.NoConflict.Mixfile, Ex_doc.NoConflict.Mixfile ]
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
end
