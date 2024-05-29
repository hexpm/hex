ExUnit.configure(exclude: [:skip | ExUnit.configuration()[:exclude]])
ExUnit.start()
Application.ensure_all_started(:bypass)

alias HexTest.Case
alias HexTest.Hexpm

# Set up temp directory
File.rm_rf!(Case.tmp_path())
File.mkdir_p!(Case.tmp_path())

Case.init_reset_state()

# Set up package fixtures
unless :integration in ExUnit.configuration()[:exclude] do
  Hexpm.init()
  Hexpm.start()

  pkg_meta = %{
    "licenses" => ["GPL-2.0", "MIT", "Apache-2.0"],
    "links" => %{"docs" => "http://docs", "repo" => "http://repo"},
    "description" => "Some description"
  }

  auth = Hexpm.new_user("user", "user@mail.com", "hunter42", "my_key")
  Hexpm.new_user("user2", "user2@mail.com", "hunter42", "my_key")

  package_name_meta = Map.put(pkg_meta, "app", "app_name")

  Hexpm.new_package("ex_doc", "0.0.1", [], pkg_meta, auth)
  Hexpm.new_package("ex_doc", "0.1.0", [], pkg_meta, auth)
  Hexpm.new_package("ex_doc", "0.1.0-rc1", [], pkg_meta, auth)
  Hexpm.new_package("postgrex", "0.2.1", [ex_doc: "~> 0.1.0"], pkg_meta, auth)
  Hexpm.new_package("postgrex", "0.2.0", [ex_doc: "0.0.1"], pkg_meta, auth)
  Hexpm.new_package("ecto", "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"], pkg_meta, auth)
  Hexpm.new_package("phoenix", "0.0.1", [postgrex: "~> 0.2"], pkg_meta, auth)
  Hexpm.new_package("only_doc", "0.1.0", [{:ex_doc, ">= 0.0.0", optional: true}], pkg_meta, auth)
  Hexpm.new_package("package_name", "0.1.0", [], package_name_meta, auth)
  Hexpm.new_package("foo", "0.1.0", [], pkg_meta, auth)
  Hexpm.new_package("foo", "0.1.1", [], pkg_meta, auth)
  Hexpm.new_package("bar", "0.1.0", [foo: "~> 0.1.0"], pkg_meta, auth)
  Hexpm.new_package("baz", "0.1.0", [foo: "0.1.0"], pkg_meta, auth)
  Hexpm.new_package("beta", "1.0.0", [], pkg_meta, auth)
  Hexpm.new_package("beta", "1.1.0-beta", [], pkg_meta, auth)
  Hexpm.new_package("tired", "0.1.0", [], pkg_meta, auth)
  Hexpm.new_package("tired", "0.2.0", [], pkg_meta, auth)

  Hexpm.new_package(
    "ecto",
    "0.2.1",
    [
      {:sample, "0.0.1", path: Case.fixture_path("sample")},
      postgrex: "~> 0.2.1",
      ex_doc: "0.1.0"
    ],
    pkg_meta,
    auth
  )

  Hexpm.new_package(
    "depend_name",
    "0.2.0",
    [{:app_name, ">= 0.0.0", hex: :package_name}],
    pkg_meta,
    auth
  )

  {:ok, _} = Hex.API.Release.retire("hexpm", "tired", "0.1.0", %{reason: "invalid"}, auth)

  Hexpm.new_repo("testorg", auth)
  Hexpm.new_package("foo", "0.1.0", [], pkg_meta, "testorg", auth)
  Hexpm.new_package("bar", "0.1.0", [foo: "~> 0.1.0"], pkg_meta, "testorg", auth)
end
