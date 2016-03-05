defmodule Mix.Tasks.Hex.OwnerTest do
  use HexTest.Case
  @moduletag :integration

  test "add owner" do
    auth1 = HexTest.HexWeb.new_user("owner_user1", "owner_user1@mail.com", "pass", "key")
    auth2 = HexTest.HexWeb.new_user("owner_user2", "owner_user2@mail.com", "pass", "key")
    HexTest.HexWeb.new_package("owner_package1", "0.0.1", [], %{}, auth1)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth1)

    Mix.Tasks.Hex.Owner.run(["add", "owner_package1", "owner_user2@mail.com"])

    assert_received {:mix_shell, :info, ["Adding owner owner_user2@mail.com to owner_package1"]}
    assert {200, %{"owned_packages" => %{"owner_package1" => _}}, _} = Hex.API.User.get("owner_user2", auth2)
  end

  test "remove owner" do
    auth = HexTest.HexWeb.new_user("owner_user3", "owner_user3@mail.com", "pass", "key")
    HexTest.HexWeb.new_user("owner_user4", "owner_user4@mail.com", "pass", "key")
    HexTest.HexWeb.new_package("owner_package2", "0.0.1", [], %{}, auth)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth)

    Mix.Tasks.Hex.Owner.run(["add", "owner_package2", "owner_user4@mail.com"])
    Mix.Tasks.Hex.Owner.run(["remove", "owner_package2", "owner_user3@mail.com"])

    assert_received {:mix_shell, :info, ["Removing owner owner_user3@mail.com from owner_package2"]}
    assert {200, %{"owned_packages" => owned}, _} = Hex.API.User.get("owner_user3", auth)
    assert owned == %{}
  end

  test "list owners" do
    auth = HexTest.HexWeb.new_user("owner_user5", "owner_user5@mail.com", "pass", "key")
    HexTest.HexWeb.new_package("owner_package3", "0.0.1", [], %{}, auth)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth)

    Mix.Tasks.Hex.Owner.run(["list", "owner_package3"])
    assert_received {:mix_shell, :info, ["owner_user5@mail.com"]}
  end
end
