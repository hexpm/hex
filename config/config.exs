use Mix.Config

if Mix.env == :test do
  config :hex_web,
    password_work_factor: 4,
    url: "http://localhost:4000",
    secret: "796f75666f756e64746865686578",
    port: "4043"

  config :hex_web, HexWeb.Repo,
    adapter: Ecto.Adapters.Postgres,
    extensions: [{HexWeb.JSON.Extension, library: Poison}],
    url: System.get_env("TEST_DATABASE_URL") ||
         "ecto://postgres:postgres@localhost/hex_test",
    size: 1,
    max_overflow: 0

  config :logger,
    level: :warn

  config :logger, :console,
    format: "$date $time [$level] $message\n"
end
