defmodule Mix.Tasks.Hex.OwnerTest do
  use HexTest.IntegrationCase

  test "add owner" do
    auth = Hexpm.new_user("owner_user1", "owner_user1@mail.com", "passpass", "key")
    Hexpm.new_user("owner_user2", "owner_user2@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "owner_package1", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    send(self(), {:mix_shell_input, :prompt, "passpass"})
    Mix.Tasks.Hex.Owner.run(["add", "owner_package1", "owner_user2@mail.com"])

    assert_received {:mix_shell, :info,
                     [
                       "Adding owner owner_user2@mail.com with ownership level full to owner_package1"
                     ]}

    assert {:ok, {200, _, %{"owned_packages" => %{"owner_package1" => _}}}} =
             Hex.API.User.get("owner_user2")
  end

  test "add owner with maintainer level" do
    auth = Hexpm.new_user("owner_user1a", "owner_user1a@mail.com", "passpass", "key")
    Hexpm.new_user("owner_user2a", "owner_user2a@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "owner_package1a", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    send(self(), {:mix_shell_input, :prompt, "passpass"})

    Mix.Tasks.Hex.Owner.run([
      "add",
      "owner_package1a",
      "owner_user2a@mail.com",
      "--level",
      "maintainer"
    ])

    assert_received {:mix_shell, :info,
                     [
                       "Adding owner owner_user2a@mail.com with ownership level maintainer to owner_package1a"
                     ]}

    assert {:ok, {200, _, %{"owned_packages" => %{"owner_package1a" => _}}}} =
             Hex.API.User.get("owner_user2a")
  end

  test "add owner with invalid level" do
    auth = Hexpm.new_user("owner_user1b", "owner_user1b@mail.com", "passpass", "key")
    Hexpm.new_user("owner_user2b", "owner_user2b@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "owner_package1b", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    send(self(), {:mix_shell_input, :prompt, "passpass"})

    assert_raise Mix.Error, "Invalid ownership level, expected one of: full, maintainer", fn ->
      Mix.Tasks.Hex.Owner.run([
        "add",
        "owner_package1b",
        "owner_user2b@mail.com",
        "--level",
        "invalid"
      ])
    end
  end

  test "add owner by username" do
    auth = Hexpm.new_user("owner_user1c", "owner_user1c@mail.com", "passpass", "key")
    Hexpm.new_user("owner_user2c", "owner_user2c@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "owner_package1c", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    send(self(), {:mix_shell_input, :prompt, "passpass"})
    Mix.Tasks.Hex.Owner.run(["add", "owner_package1c", "owner_user2c"])

    assert_received {:mix_shell, :info,
                     [
                       "Adding owner owner_user2c with ownership level full to owner_package1c"
                     ]}

    assert {:ok, {200, _, %{"owned_packages" => %{"owner_package1c" => _}}}} =
             Hex.API.User.get("owner_user2c")
  end

  test "remove owner" do
    auth = Hexpm.new_user("owner_user3", "owner_user3@mail.com", "passpass", "key")
    Hexpm.new_user("owner_user4", "owner_user4@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "owner_package2", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    send(self(), {:mix_shell_input, :prompt, "passpass"})
    send(self(), {:mix_shell_input, :prompt, "passpass"})
    Mix.Tasks.Hex.Owner.run(["add", "owner_package2", "owner_user4@mail.com"])
    Mix.Tasks.Hex.Owner.run(["remove", "owner_package2", "owner_user3@mail.com"])

    assert_received {:mix_shell, :info,
                     ["Removing owner owner_user3@mail.com from owner_package2"]}

    assert {:ok, {200, _, %{"owned_packages" => owned}}} = Hex.API.User.get("owner_user3")
    assert owned == %{}
  end

  test "list owners" do
    auth = Hexpm.new_user("owner_user5", "owner_user5@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "owner_package3", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    Mix.Tasks.Hex.Owner.run(["list", "owner_package3"])

    output =
      [
        ["owner_user5@mail.com", :reset, "  "],
        ["full", :reset, "   "]
      ]
      |> IO.ANSI.format()
      |> List.to_string()

    assert_received {:mix_shell, :info, [^output]}
  end

  test "list all packages owned by the current user" do
    package1 = "owner_package4"
    package2 = "owner_package5"
    owner_email = "owner_user6@mail.com"
    auth = Hexpm.new_user("owner_user6", owner_email, "passpass", "key")
    Hexpm.new_package("hexpm", package1, "0.0.1", [], %{}, auth)
    Hexpm.new_package("hexpm", package2, "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    Mix.Tasks.Hex.Owner.run(["packages"])
    owner_package4_msg = "#{package1} - http://localhost:4043/packages/#{package1}"
    owner_package5_msg = "#{package2} - http://localhost:4043/packages/#{package2}"
    assert_received {:mix_shell, :info, [^owner_package4_msg]}
    assert_received {:mix_shell, :info, [^owner_package5_msg]}
  end

  test "transfer owner by username" do
    auth = Hexpm.new_user("owner_user7a", "owner_user7a@mail.com", "passpass", "key")
    Hexpm.new_user("owner_user7b", "owner_user7b@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "owner_package6", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Hex.State.put(:api_key, auth[:key])

    send(self(), {:mix_shell_input, :prompt, "passpass"})
    Mix.Tasks.Hex.Owner.run(["transfer", "owner_package6", "owner_user7b"])

    assert_received {:mix_shell, :info,
                     [
                       "Transferring ownership to owner_user7b for owner_package6"
                     ]}

    assert {:ok, {200, _, %{"owned_packages" => packages_a}}} = Hex.API.User.get("owner_user7a")
    assert {:ok, {200, _, %{"owned_packages" => packages_b}}} = Hex.API.User.get("owner_user7b")

    assert Map.keys(packages_a) == []
    assert Map.keys(packages_b) == ["owner_package6"]
  end
end
