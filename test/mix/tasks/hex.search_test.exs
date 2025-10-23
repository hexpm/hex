defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.IntegrationCase

  describe "hexdocs" do
    test "no args" do
      Mix.Tasks.Hex.Search.run([])
      assert_received {:hex_system_cmd, _, args}
      vsn = Application.spec(:plug, :vsn)
      assert "https://hexdocs.pm/?packages=" <> packages = List.last(args)
      assert packages =~ URI.encode_www_form("plug:#{vsn},")
      assert String.ends_with?(packages, "&q=")
    end
  end

  describe "package" do
    test "backwards compatibility" do
      Mix.Tasks.Hex.Search.run(["bloopdoopbloop"])

      assert_received {:mix_shell, :error,
                       ["mix hex.search PACKAGE is deprecated, use --package PACKAGE instead"]}

      assert_received {:mix_shell, :info, ["No packages found"]}
    end

    test "no results" do
      Mix.Tasks.Hex.Search.run(["--package", "bloopdoopbloop"])
      assert_received {:mix_shell, :info, ["No packages found"]}
    end

    test "public packages" do
      Mix.Tasks.Hex.Search.run(["--package", "doc"])
      assert_received {:mix_shell, :info, ["ex_doc" <> ex_doc]}
      assert_received {:mix_shell, :info, ["only_doc" <> only_doc]}
      assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
      assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
    end

    test "all private packages" do
      in_tmp(fn ->
        set_home_tmp()
        auth = Hexpm.new_user("searchuser1", "searchuser1@mail.com", "password", "searchuser1")
        Hexpm.new_repo("searchrepo1", auth)
        Hex.State.put(:api_key, auth[:key])

        Mix.Tasks.Hex.Search.run(["--package", "doc"])

        assert_received {:mix_shell, :info, ["ex_doc" <> ex_doc]}
        assert_received {:mix_shell, :info, ["only_doc" <> only_doc]}
        assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
        assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
      end)
    end

    test "org packages" do
      in_tmp(fn ->
        set_home_tmp()
        auth = Hexpm.new_user("searchuser2", "searchuser2@mail.com", "password", "searchuser2")
        Hexpm.new_repo("searchrepo2", auth)
        Hex.State.put(:api_key, auth[:key])

        Mix.Tasks.Hex.Search.run(["--package", "doc", "--organization", "searchrepo2"])

        refute_received {:mix_shell, :info, ["ex_doc" <> _]}
        refute_received {:mix_shell, :info, ["only_doc" <> _]}
      end)
    end
  end
end
