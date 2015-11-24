ExUnit.start exclude: [:skip]

alias HexTest.Case
File.rm_rf!(Case.tmp_path)
File.mkdir_p!(Case.tmp_path)

Application.start(:logger)
Hex.State.put(:api, "http://localhost:4043/api")
unless System.get_env("HEX_CDN"), do: Hex.State.put(:cdn, "http://localhost:4043")


unless :integration in ExUnit.configuration[:exclude] do
  db = "hex_test"
  db_url = "ecto://postgres:postgres@localhost/#{db}"

  System.put_env("DATABASE_URL", db_url)

  File.cd! "_build/test/lib/hex_web", fn ->
    Mix.Task.run "ecto.drop", ["-r", "HexWeb.Repo"]
    Mix.Task.run "ecto.create", ["-r", "HexWeb.Repo"]
    Mix.Task.run "ecto.migrate", ["-r", "HexWeb.Repo"]
  end
  HexWeb.Repo.stop

  {:ok, _} = Application.ensure_all_started(:hex_web)

  pkg_meta = %{
    "maintainers" => ["John Doe", "Jane Doe"],
    "licenses" => ["GPL2", "MIT", "Apache"],
    "links" => %{"docs" => "http://docs", "repo" => "http://repo"},
    "description" => "builds docs"}

  rel_meta = %{
    "app" => "ex_doc",
    "build_tools" => ["mix"]
  }

  {:ok, user}    = HexWeb.User.create(%{"username" => "user", "email" => "user@mail.com", "password" => "hunter42"})
  {:ok, package} = HexWeb.Package.create(user, %{"name" => "ex_doc", "meta" => pkg_meta})
  {:ok, _}       = HexWeb.Release.create(package, %{"version" => "0.0.1", "requirements" => %{}, "meta" => rel_meta}, "")

  HexWeb.User.confirm(user)

  {201, %{"secret" => secret}} = Hex.API.Key.new("my_key", [user: "user", pass: "hunter42"])
  auth = [key: secret]

  Case.init_project("ex_doc", "0.0.1", [], pkg_meta, auth)
  Case.init_project("ex_doc", "0.0.1", [], pkg_meta, auth)
  Case.init_project("ex_doc", "0.1.0", [], pkg_meta, auth)
  Case.init_project("postgrex", "0.2.1", [ex_doc: "~> 0.1.0"], %{}, auth)
  Case.init_project("postgrex", "0.2.0", [ex_doc: "0.0.1"], %{}, auth)
  Case.init_project("ecto", "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"], %{}, auth)
  Case.init_project("ecto", "0.2.1", [{:sample, "0.0.1", path: Case.fixture_path("sample")}, postgrex: "~> 0.2.1", ex_doc: "0.1.0"], %{}, auth)
  Case.init_project("phoenix", "0.0.1", [postgrex: "~> 0.2"], %{}, auth)
  Case.init_project("only_doc", "0.1.0", [{:ex_doc, ">= 0.0.0", optional: true}], %{}, auth)
  Case.init_project("package_name", "0.1.0", [], %{app: "app_name"}, auth)
  Case.init_project("depend_name", "0.2.0", [{:app_name, ">= 0.0.0", optional: true, hex: :package_name}], %{}, auth)
  Case.init_project("foo", "0.1.0", [], pkg_meta, auth)
  Case.init_project("bar", "0.1.0", [foo: "~> 0.1.0"], pkg_meta, auth)
end
