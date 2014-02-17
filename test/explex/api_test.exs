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

    Explex.start_api()
    Explex.url("http://localhost:4000")

    :application.ensure_all_started(:explex_web)
    :ok
  end

  teardown_all do
    :inets.stop()
    :application.stop(:explex_web)
  end

  setup_all do
    { 201, _ } = Explex.API.new_user("user", "user@mail.com", "hunter42")
    :ok
  end

  test "user" do
    assert { 404, _ } = Explex.API.get_user("test_user")
    assert { 201, _ } = Explex.API.new_user("test_user", "test_user@mail.com", "hunter42")
    assert { 200, body } = Explex.API.get_user("test_user")
    assert body["username"] == "test_user"
  end

  test "package" do
    auth = [user: "user", password: "hunter42"]

    assert { 404, _ } = Explex.API.get_package("ecto")
    assert { 201, _ } = Explex.API.new_package("ecto", [description: "foobar"], auth)
    assert { 200, body } = Explex.API.get_package("ecto")
    assert body["meta"]["description"] == "foobar"
  end

  test "release" do
    auth = [user: "user", password: "hunter42"]
    Explex.API.new_package("postgrex", [], auth)
    Explex.API.new_package("decimal", [], auth)

    assert { 404, _ } = Explex.API.get_release("postgrex", "0.0.1")
    assert { 201, _ } = Explex.API.new_release("postgrex", "0.0.1", "url", "ref", [], auth)
    assert { 200, body } = Explex.API.get_release("postgrex", "0.0.1")
    assert body["git_url"] == "url"
    assert body["git_ref"] == "ref"
    assert body["requirements"] == []

    reqs = [{ "postgrex", "~> 0.0.1" }]
    assert { 201, _ } = Explex.API.new_release("decimal", "0.0.2", "url", "ref", reqs, auth)
    assert { 200, body } = Explex.API.get_release("decimal", "0.0.2")
    assert body["requirements"] == reqs
  end

  test "registry" do
    Explex.API.get_registry("tmp/file.dets")
    assert File.exists?("tmp/file.dets")
  end
end
