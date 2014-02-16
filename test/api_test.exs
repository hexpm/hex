defmodule Explex.APITest do
  use ExplexTest.Case
  @moduletag :api

  @db "explex_client_test"
  @db_url "ecto://explex:explex@localhost/#{@db}"

  setup_all do
    System.put_env("EXPLEX_ALT_ECTO_URL", @db_url)

    setup_cmds = [
      ~s(psql -U explex -c "DROP DATABASE IF EXISTS #{@db};"),
      ~s(psql -U explex -c "CREATE DATABASE #{@db} ENCODING='UTF8' LC_COLLATE='en_US.UTF-8' LC_CTYPE='en_US.UTF-8';"),
      ~s(cd ../explex_web && mix ecto.migrate ExplexWeb.Repo)
    ]

    Enum.find_value(setup_cmds, fn cmd ->
      status = Mix.Shell.cmd(cmd, fn x -> x end)

      if status != 0 do
        IO.puts "Test setup command error'd: `#{cmd}`"
        :error
      end
    end) || :ok
  end

  setup_all do
    Explex.url("http://localhost:4000")

    "../explex_web/_build/dev/lib/*/ebin"
    |> Path.wildcard
    |> Enum.each(&Code.prepend_path/1)

    :inets.start()
    :application.ensure_all_started(:explex_web)
  end

  teardown_all do
    :inets.stop()
    :application.stop(:explex_web)
  end

  test "user" do
    assert { 404, _ } = Explex.API.get_user("ericmj")
    assert { 201, _ } = Explex.API.new_user("ericmj", "eric@mail.com", "hunter42")
    assert { 200, body } = Explex.API.get_user("ericmj")
    assert body["username"] == "ericmj"
  end
end
