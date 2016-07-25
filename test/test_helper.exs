ExUnit.start exclude: [:skip]
Application.ensure_all_started(:bypass)

alias HexTest.Case
alias HexTest.HexWeb

# Set up temp directory
File.rm_rf!(Case.tmp_path)
File.mkdir_p!(Case.tmp_path)

Case.reset_state()

# Set up package fixtures
unless :integration in ExUnit.configuration[:exclude] do
  HexWeb.init
  HexWeb.start

  pkg_meta = %{
    "maintainers" => ["John Doe", "Jane Doe"],
    "licenses" => ["GPL2", "MIT", "Apache"],
    "links" => %{"docs" => "http://docs", "repo" => "http://repo"},
    "description" => "Some description"
  }

  auth = HexWeb.new_user("user", "user@mail.com", "hunter42", "my_key")
  package_name_meta = Map.put(pkg_meta, "app", "app_name")

  HexWeb.new_package("ex_doc", "0.0.1", [], pkg_meta, auth)
  HexWeb.new_package("ex_doc", "0.1.0", [], pkg_meta, auth)
  HexWeb.new_package("ex_doc", "0.1.0-rc1", [], pkg_meta, auth)
  HexWeb.new_package("postgrex", "0.2.1", [ex_doc: "~> 0.1.0"], pkg_meta, auth)
  HexWeb.new_package("postgrex", "0.2.0", [ex_doc: "0.0.1"], pkg_meta, auth)
  HexWeb.new_package("ecto", "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"], pkg_meta, auth)
  HexWeb.new_package("ecto", "0.2.1", [{:sample, "0.0.1", path: Case.fixture_path("sample")}, postgrex: "~> 0.2.1", ex_doc: "0.1.0"], pkg_meta, auth)
  HexWeb.new_package("phoenix", "0.0.1", [postgrex: "~> 0.2"], pkg_meta, auth)
  HexWeb.new_package("only_doc", "0.1.0", [{:ex_doc, ">= 0.0.0", optional: true}], pkg_meta, auth)
  HexWeb.new_package("package_name", "0.1.0", [], package_name_meta, auth)
  HexWeb.new_package("depend_name", "0.2.0", [{:app_name, ">= 0.0.0", hex: :package_name}], pkg_meta, auth)
  HexWeb.new_package("foo", "0.1.0", [], pkg_meta, auth)
  HexWeb.new_package("foo", "0.1.1", [], pkg_meta, auth)
  HexWeb.new_package("bar", "0.1.0", [foo: "~> 0.1.0"], pkg_meta, auth)
  HexWeb.new_package("beta", "1.0.0", [], pkg_meta, auth)
  HexWeb.new_package("beta", "1.1.0-beta", [], pkg_meta, auth)
end
