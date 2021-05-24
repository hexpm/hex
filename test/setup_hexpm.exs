alias HexTest.Case
alias HexTest.Hexpm
Hexpm.init()
Hexpm.start()

pkg_meta = %{
  "licenses" => ["GPL2", "MIT", "Apache"],
  "links" => %{"docs" => "http://docs", "repo" => "http://repo"},
  "description" => "Some description"
}

auth = Hexpm.new_user("user", "user@mail.com", "hunter42", "my_key")
Hexpm.new_user("user2", "user2@mail.com", "hunter42", "my_key")

package_name_meta = Map.put(pkg_meta, "app", "app_name")

Hexpm.new_package("hexpm", "ex_doc", "0.0.1", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "ex_doc", "0.1.0", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "ex_doc", "0.1.0-rc1", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "postgrex", "0.2.1", [ex_doc: "~> 0.1.0"], pkg_meta, auth)
Hexpm.new_package("hexpm", "postgrex", "0.2.0", [ex_doc: "0.0.1"], pkg_meta, auth)

Hexpm.new_package(
  "hexpm",
  "ecto",
  "0.2.0",
  [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"],
  pkg_meta,
  auth
)

Hexpm.new_package("hexpm", "phoenix", "0.0.1", [postgrex: "~> 0.2"], pkg_meta, auth)

Hexpm.new_package(
  "hexpm",
  "only_doc",
  "0.1.0",
  [{:ex_doc, ">= 0.0.0", optional: true}],
  pkg_meta,
  auth
)

Hexpm.new_package("hexpm", "package_name", "0.1.0", [], package_name_meta, auth)
Hexpm.new_package("hexpm", "foo", "0.1.0", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "foo", "0.1.1", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "bar", "0.1.0", [foo: "~> 0.1.0"], pkg_meta, auth)
Hexpm.new_package("hexpm", "baz", "0.1.0", [foo: "0.1.0"], pkg_meta, auth)
Hexpm.new_package("hexpm", "beta", "1.0.0", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "beta", "1.1.0-beta", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "tired", "0.1.0", [], pkg_meta, auth)
Hexpm.new_package("hexpm", "tired", "0.2.0", [], pkg_meta, auth)

Hexpm.new_package(
  "hexpm",
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
  "hexpm",
  "depend_name",
  "0.2.0",
  [{:app_name, ">= 0.0.0", hex: :package_name}],
  pkg_meta,
  auth
)

{:ok, _} = Hex.API.Release.retire("hexpm", "tired", "0.1.0", %{reason: "invalid"}, auth)

Hexpm.new_repo("testorg", auth)
Hexpm.new_package("testorg", "foo", "0.1.0", [], pkg_meta, auth)
Hexpm.new_package("testorg", "bar", "0.1.0", [foo: "~> 0.1.0"], pkg_meta, auth)

Hexpm.new_package("hexpm", "ecto_sql", "3.3.2", [ecto: "~> 3.3.1"], pkg_meta, auth, [
  {"mix.exs", File.read!(Case.fixture_path("ecto_sql_3_3_2/mix.exs"))}
])

Hexpm.new_package("hexpm", "ecto_sql", "3.3.3", [ecto: "~> 3.3.2"], pkg_meta, auth, [
  {"mix.exs", File.read!(Case.fixture_path("ecto_sql_3_3_3/mix.exs"))}
])

Hexpm.new_package("hexpm", "ecto_enum", "1.4.0", [ecto: ">= 3.0.0"], pkg_meta, auth, [
  {"mix.exs", File.read!(Case.fixture_path("ecto_enum_1_4_0/mix.exs"))}
])

Hexpm.new_package("hexpm", "ecto", "3.3.1", [], pkg_meta, auth, [
  {"mix.exs", File.read!(Case.fixture_path("ecto_3_3_1/mix.exs"))}
])

Hexpm.new_package("hexpm", "ecto", "3.3.2", [], pkg_meta, auth, [
  {"mix.exs", File.read!(Case.fixture_path("ecto_3_3_2/mix.exs"))}
])

sponsored_meta = put_in(pkg_meta, ["links", "Sponsor"], "https://my.sponsor.link")
Hexpm.new_package("hexpm", "sponsored", "0.1.0", [], sponsored_meta, auth)
