defmodule Mix.Tasks.Hex.OwnerTest do
  use HexTest.Case
  @moduletag :integration

  test "add owner" do
    auth1 = HexWeb.new_user("owner_user1", "owner_user1@mail.com", "pass", "key")
    auth2 = HexWeb.new_user("owner_user2", "owner_user2@mail.com", "pass", "key")
    HexWeb.new_package("owner_package1", "0.0.1", [], %{}, auth1)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth1)

    send self(), {:mix_shell_input, :prompt, "pass"}
    Mix.Tasks.Hex.Owner.run(["add", "owner_package1", "owner_user2@mail.com"])

    assert_received {:mix_shell, :info, ["Adding owner owner_user2@mail.com to owner_package1"]}
    assert {200, %{"owned_packages" => %{"owner_package1" => _}}, _} = Hex.API.User.get("owner_user2", auth2)
  end

  test "remove owner" do
    auth = HexWeb.new_user("owner_user3", "owner_user3@mail.com", "pass", "key")
    HexWeb.new_user("owner_user4", "owner_user4@mail.com", "pass", "key")
    HexWeb.new_package("owner_package2", "0.0.1", [], %{}, auth)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth)

    send self(), {:mix_shell_input, :prompt, "pass"}
    send self(), {:mix_shell_input, :prompt, "pass"}
    Mix.Tasks.Hex.Owner.run(["add", "owner_package2", "owner_user4@mail.com"])
    Mix.Tasks.Hex.Owner.run(["remove", "owner_package2", "owner_user3@mail.com"])

    assert_received {:mix_shell, :info, ["Removing owner owner_user3@mail.com from owner_package2"]}
    assert {200, %{"owned_packages" => owned}, _} = Hex.API.User.get("owner_user3", auth)
    assert owned == %{}
  end

  test "list owners" do
    auth = HexWeb.new_user("owner_user5", "owner_user5@mail.com", "pass", "key")
    HexWeb.new_package("owner_package3", "0.0.1", [], %{}, auth)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth)

    send self(), {:mix_shell_input, :prompt, "pass"}
    Mix.Tasks.Hex.Owner.run(["list", "owner_package3"])
    assert_received {:mix_shell, :info, ["owner_user5@mail.com"]}
  end

  test "list all packages owned by the current user" do
    package1 = "owner_package4"
    package2 = "owner_package5"
    owner_email = "owner_user6@mail.com"
    auth = HexWeb.new_user("owner_user6", owner_email, "pass", "key")
    HexWeb.new_package(package1, "0.0.1", [], %{}, auth)
    HexWeb.new_package(package2, "0.0.1", [], %{}, auth)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth)

    send self(), {:mix_shell_input, :prompt, "pass"}
    send self(), {:mix_shell_input, :prompt, "pass"}
    send self(), {:mix_shell_input, :prompt, "pass"}
    Mix.Tasks.Hex.Owner.run(["add", package1, owner_email])
    Mix.Tasks.Hex.Owner.run(["add", package2, owner_email])

    Mix.Tasks.Hex.Owner.run(["packages"])
    owner_package4_msg = "#{package1} - http://localhost:4043/packages/#{package1}"
    owner_package5_msg = "#{package2} - http://localhost:4043/packages/#{package2}"
    assert_received {:mix_shell, :info, [^owner_package4_msg]}
    assert_received {:mix_shell, :info, [^owner_package5_msg]}
  end
end
