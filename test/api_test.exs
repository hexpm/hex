defmodule Explex.APITest do
  use ExplexTest.Case
  @moduletag :api

  @db "explex_client_test"
  @db_url "ecto://explex:explex@localhost/#{@db}"

  defp reset_db(repo, dir) do
    files = Path.join(dir, "*") |> Path.wildcard

    modules =
      Enum.map(files, fn file ->
        filename = Path.basename(file)
        { version, _ } = Integer.parse(filename)
        [{ module, _ }] = Code.load_file(file)
        { version, module }
      end) |> Enum.sort

    Enum.each(Enum.reverse(modules), fn { version, module } ->
      :ok = Ecto.Migrator.down(repo, version, module)
    end)

    Enum.each(modules, fn { version, module } ->
      :ok = Ecto.Migrator.up(repo, version, module)
    end)
  end

  setup_all do
    System.put_env("EXPLEX_ECTO_URL", @db_url)
    { :ok, _ } = ExplexWeb.Repo.start_link
    reset_db(ExplexWeb.Repo, "deps/explex_web/priv/migrations")
    ExplexWeb.Repo.stop

    Explex.url("http://localhost:4000")

    :inets.start()
    :application.ensure_all_started(:explex_web)
    :ok
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
