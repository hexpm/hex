defmodule Explex.MixTest do
  use ExplexTest.Case

  defmodule Foo do
    def project do
      [ app: :foo,
        version: "0.1.0",
        deps: [ { :ecto, [package: true] } ] ]
    end
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
end
