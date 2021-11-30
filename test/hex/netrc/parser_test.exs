defmodule Hex.Netrc.ParserTest do
  use HexTest.Case, async: false

  alias Hex.Netrc.Parser

  test "parse/1 fails on non-existent file" do
    in_tmp(fn ->
      assert {:error, :enoent} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 fails on unreadable file" do
    in_tmp(fn ->
      File.write!(".netrc", "")
      File.chmod!(".netrc", 0o000)
      assert {:error, :eacces} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 fails on empty file" do
    in_tmp(fn ->
      File.write!(".netrc", "")
      assert {:error, :parse} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 succeeds on simple file" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
        login john
        password bar
      """

      parsed = %{
        "foo.example.com" => %{
          username: "john",
          password: "bar"
        }
      }

      File.write!(".netrc", data)
      assert {:ok, ^parsed} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 succeeds on file with multiple records" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
        login john
        password bar
      machine bar.example.com
        login yoyo
        password dyne
      """

      parsed = %{
        "foo.example.com" => %{
          username: "john",
          password: "bar"
        },
        "bar.example.com" => %{
          username: "yoyo",
          password: "dyne"
        }
      }

      File.write!(".netrc", data)
      assert {:ok, ^parsed} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 ignores excessive whitespace" do
    in_tmp(fn ->
      data = "\n\n\t\nmachine foo.example.com\n\n     login\t\tjohn\npassword\t\tbar\n\n"

      parsed = %{
        "foo.example.com" => %{
          username: "john",
          password: "bar"
        }
      }

      File.write!(".netrc", data)
      assert {:ok, ^parsed} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 missing machine line is a parse error" do
    in_tmp(fn ->
      data = """
      login foo
      password bar
      """

      File.write!(".netrc", data)
      assert {:error, :parse} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 missing login line is a parse error" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
      password bar
      """

      File.write!(".netrc", data)
      assert {:error, :parse} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 missing password line is a parse error" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
      login foo
      """

      File.write!(".netrc", data)
      assert {:error, :parse} = Parser.parse(".netrc")
    end)
  end

  test "parse/1 username can be overridden" do
    in_tmp(fn ->
      data = """
      machine foo.example.com
      login foo
      login foo2
      password bar
      """

      parsed = %{
        "foo.example.com" => %{
          username: "foo2",
          password: "bar"
        }
      }

      File.write!(".netrc", data)
      assert {:ok, ^parsed} = Parser.parse(".netrc")
    end)
  end
end
