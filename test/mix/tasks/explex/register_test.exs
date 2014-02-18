defmodule Mix.Tasks.Explex.RegisterTest do
  use ExplexTest.Case
  @moduletag :integration

  test "validate" do
    assert_raise Mix.Error, "Missing command line option: email", fn ->
      Mix.Tasks.Explex.Register.run(["-u", "validation_name", "-p", "hunter42"])
    end
  end

  test "create" do
    Mix.Tasks.Explex.Register.run(["-u", "validation_name", "-p", "hunter42", "--email", "mail"])
    assert_received { :mix_shell, :info, ["Registration of user validation_name successful!"] }

    Mix.Tasks.Explex.Register.run(["-u", "some_other_name", "-p", "hunter42", "--email", "mail"])
    assert_received { :mix_shell, :error, ["Registration of user some_other_name failed! (422)"] }
  end
end
