defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.IntegrationCase
  import Mox

  setup :verify_on_exit!

  defmodule SearchDeps.MixProject do
    def project do
      [
        app: :search_app,
        version: "0.0.1",
        deps: [
          {:foo, "0.1.0"}
        ]
      ]
    end
  end

  describe "hexdocs" do
    test "no args" do
      Mix.Project.push(SearchDeps.MixProject)

      in_tmp(fn ->
        write_search_deps()
        Mix.Tasks.Hex.Search.run([])
        assert_received {:hex_system_cmd, _, ["https://hexdocs.pm/?packages=" <> packages]}

        vsn = System.version()

        assert packages =~
                 URI.encode_www_form(
                   "bar:0.1.0,eex:#{vsn},elixir:#{vsn},ex_unit:#{vsn},foo:0.1.0"
                 )

        assert String.ends_with?(packages, "&q=")
      end)
    end

    test "--no-stdlib" do
      Mix.Project.push(SearchDeps.MixProject)

      in_tmp(fn ->
        write_search_deps()
        Mix.Tasks.Hex.Search.run(["--no-stdlib"])
        assert_received {:hex_system_cmd, _, ["https://hexdocs.pm/?packages=" <> packages]}

        assert packages =~ URI.encode_www_form("bar:0.1.0,foo:0.1.0")
        assert String.ends_with?(packages, "&q=")
      end)
    end

    test "--print-url" do
      Mix.Project.push(SearchDeps.MixProject)

      in_tmp(fn ->
        write_search_deps()
        Mix.Tasks.Hex.Search.run(["--print-url"])
        assert_received {:mix_shell, :info, ["https://hexdocs.pm/?packages=" <> packages]}

        vsn = System.version()

        assert packages =~
                 URI.encode_www_form(
                   "bar:0.1.0,eex:#{vsn},elixir:#{vsn},ex_unit:#{vsn},foo:0.1.0"
                 )

        assert String.ends_with?(packages, "&q=")
      end)
    end

    test "--packages opens explicit package list" do
      Mix.Tasks.Hex.Search.run(["--packages", "foo,bar"])

      assert_received {:hex_system_cmd, _, ["https://hexdocs.pm/?packages=foo%2Cbar&q="]}
    end

    test "--packages with --print-url prints explicit package list" do
      Mix.Tasks.Hex.Search.run(["--packages", "foo,bar", "--print-url"])

      assert_received {:mix_shell, :info, ["https://hexdocs.pm/?packages=foo%2Cbar&q="]}
    end

    defp write_search_deps do
      set_home_tmp()
      Mix.Dep.Lock.write(%{foo: {:hex, :foo, "0.1.0"}, bar: {:hex, :bar, "0.1.0"}})
      Mix.Task.run("deps.get")
      flush()
    end
  end

  describe "docs query" do
    setup do
      if System.otp_release() >= "27" do
        :ok
      else
        {:skip, "docs query tests require OTP 27 or later"}
      end
    end

    test "prints formatted results for project dependencies" do
      Mix.Project.push(SearchDeps.MixProject)

      in_tmp(fn ->
        write_search_deps()

        mock_search_http(fn url ->
          assert URI.parse(url).host == "search.hexdocs.pm"

          query = URI.decode_query(URI.parse(url).query)
          assert query["q"] == "ecto changeset"
          assert query["query_by"] == "doc,title"
          refute Map.has_key?(query, "per_page")

          vsn = System.version()

          assert query["filter_by"] ==
                   "package:=[bar-0.1.0,eex-#{vsn},elixir-#{vsn},ex_unit-#{vsn},foo-0.1.0,iex-#{vsn},logger-#{vsn},mix-#{vsn}]"

          {:ok,
           {200, %{},
            ~s({"found":1,"hits":[{"document":{"doc":"Cast changesets.","package":"ecto-3.13.4","ref":"Ecto.Changeset.html#cast/4","title":"cast/4"}}]})}}
        end)

        Mix.Tasks.Hex.Search.run(["ecto changeset"])

        assert_received {:mix_shell, :info, [message]}

        assert message =~
                 "# cast/4 (1/1)\nhttps://hexdocs.pm/ecto/3.13.4/Ecto.Changeset.html#cast/4\n\nCast changesets.\n\n"
      end)
    end

    test "uses explicit package filters" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn url ->
          query = URI.decode_query(URI.parse(url).query)
          assert query["q"] == "query"
          assert query["filter_by"] == "package:=[foo-0.1.1,bar-0.1.0]"
          {:ok, {200, %{}, ~s({"found":0,"hits":[]})}}
        end)

        Mix.Tasks.Hex.Search.run(["query", "--packages", "foo,bar"])

        assert_received {:mix_shell, :info, ["No results found"]}
      end)
    end

    test "builds urls by splitting package on the first dash" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn _url ->
          {:ok,
           {200, %{},
            ~s({"found":1,"hits":[{"document":{"doc":"Tree docs.","package":"mix-1.20.0-rc.5","ref":"Mix.Tasks.Deps.Tree.html#module-examples","title":"Examples - mix deps.tree"}}]})}}
        end)

        Mix.Tasks.Hex.Search.run(["deps tree", "--packages", "mix"])

        assert_received {:mix_shell, :info, [message]}

        assert message =~
                 "# Examples - mix deps.tree (1/1)\nhttps://hexdocs.pm/mix/1.20.0-rc.5/Mix.Tasks.Deps.Tree.html#module-examples\n\nTree docs.\n\n"
      end)
    end

    test "includes the total result count in the heading" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn _url ->
          {:ok,
           {200, %{},
            ~s({"found":2,"hits":[{"document":{"doc":"First","package":"ecto-3.13.4","ref":"Ecto.Changeset.html#cast/4","title":"cast/4"}},{"document":{"doc":"Second","package":"ecto-3.13.4","ref":"Ecto.Changeset.html#change/2","title":"change/2"}}]})}}
        end)

        Mix.Tasks.Hex.Search.run(["query", "--packages", "foo"])

        assert_received {:mix_shell, :info, [first]}
        assert_received {:mix_shell, :info, [second]}

        assert first =~
                 "# cast/4 (1/2)\nhttps://hexdocs.pm/ecto/3.13.4/Ecto.Changeset.html#cast/4\n\nFirst\n\n"

        assert second =~
                 "# change/2 (2/2)\nhttps://hexdocs.pm/ecto/3.13.4/Ecto.Changeset.html#change/2\n\nSecond\n\n"
      end)
    end

    test "strips html comments from results" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn _url ->
          {:ok,
           {200, %{},
            ~s({"found":1,"hits":[{"document":{"doc":"Alpha<!-- internal -->Beta<!-- remove me -->Gamma","package":"ecto-3.13.4","ref":"Ecto.Changeset.html#cast/4","title":"cast/4"}}]})}}
        end)

        Mix.Tasks.Hex.Search.run(["query", "--packages", "foo"])

        assert_received {:mix_shell, :info, [message]}
        assert message =~ "\n\nAlphaBetaGamma\n\n"
        refute message =~ "<!--"
        refute message =~ "internal"
        refute message =~ "remove me"
      end)
    end

    test "uses configurable limit" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn url ->
          query = URI.decode_query(URI.parse(url).query)
          assert query["q"] == "query"
          assert query["per_page"] == "25"
          {:ok, {200, %{}, ~s({"found":0,"hits":[]})}}
        end)

        Mix.Tasks.Hex.Search.run(["query", "--packages", "foo", "--limit", "25"])

        assert_received {:mix_shell, :info, ["No results found"]}
      end)
    end

    test "raises on non-200 responses" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn _url ->
          {:ok, {500, %{}, "oops"}}
        end)

        assert_raise Mix.Error, ~r/Docs search failed with HTTP status 500/, fn ->
          Mix.Tasks.Hex.Search.run(["query", "--packages", "foo"])
        end
      end)
    end

    test "raises on transport errors" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn _url ->
          {:error, :econnrefused}
        end)

        assert_raise Mix.Error, ~r/Docs search request failed: :econnrefused/, fn ->
          Mix.Tasks.Hex.Search.run(["query", "--packages", "foo"])
        end
      end)
    end

    test "raises on invalid json" do
      in_tmp(fn ->
        set_home_tmp()

        mock_search_http(fn _url ->
          {:ok, {200, %{}, "{"}}
        end)

        assert_raise Mix.Error, ~r/Docs search returned invalid JSON: :unexpected_end/, fn ->
          Mix.Tasks.Hex.Search.run(["query", "--packages", "foo"])
        end
      end)
    end

    test "rejects incompatible query arguments" do
      assert_raise Mix.Error, ~r/mix hex.search QUERY/, fn ->
        Mix.Tasks.Hex.Search.run(["query", "--print-url"])
      end

      assert_raise Mix.Error, ~r/mix hex.search QUERY/, fn ->
        Mix.Tasks.Hex.Search.run(["--limit", "25"])
      end
    end

    test "rejects invalid limits" do
      assert_raise Mix.Error, ~r/Expected --limit to be between 1 and 250/, fn ->
        Mix.Tasks.Hex.Search.run(["query", "--limit", "0"])
      end

      assert_raise Mix.Error, ~r/Expected --limit to be between 1 and 250/, fn ->
        Mix.Tasks.Hex.Search.run(["query", "--limit", "251"])
      end
    end

    defp mock_search_http(fun) do
      Application.put_env(:hex, :search_http_module, Hex.HTTP.Mock)

      on_exit(fn ->
        Application.delete_env(:hex, :search_http_module)
      end)

      expect(Hex.HTTP.Mock, :request, fn :get, url, %{}, nil, %{} ->
        fun.(url)
      end)
    end
  end
end
