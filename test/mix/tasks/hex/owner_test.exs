defmodule Mix.Tasks.Hex.OwnerTest do
  use HexTest.Case
  @moduletag :integration

  test "add owner" do
    { :ok, user }    = HexWeb.User.create("owner_user", "owner_user@mail.com", "hunter42")
    { :ok, package } = HexWeb.Package.create("owner_package", user, %{})

    Mix.Tasks.Hex.Owner.run(["add", "owner_package", "user@mail.com", "-u", "owner_user", "-p", "hunter42"])

    assert_received { :mix_shell, :info, ["Adding owner user@mail.com to owner_package"] }
    assert [%HexWeb.User{username: "user"}, %HexWeb.User{username: "owner_user"}] =
           HexWeb.Package.owners(package)
  end

  test "remove owner" do
    user = HexWeb.User.get(username: "user")
    { :ok, package } = HexWeb.Package.create("owner_package2", user, %{})

    Mix.Tasks.Hex.Owner.run(["remove", "owner_package2", "user@mail.com", "-u", "user", "-p", "hunter42"])

    assert_received { :mix_shell, :info, ["Removing owner user@mail.com from owner_package2"] }
    assert [] = HexWeb.Package.owners(package)
  end

  test "list owners" do
    Mix.Tasks.Hex.Owner.run(["list", "ex_doc", "-u", "user", "-p", "hunter42"])
    assert_received { :mix_shell, :info, ["user@mail.com"] }
  end
end
