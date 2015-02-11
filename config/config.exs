use Mix.Config

if Mix.env == :test do
  config :hex_web,
    password_work_factor: 4,
    url: "http://localhost:4000",
    secret: "796f75666f756e64746865686578",
    database_url: System.get_env("TEST_DATABASE_URL") || "ecto://postgres:postgres@localhost/hexweb_test"

  config :logger,
    level: :warn

  config :logger, :console,
    format: "$date $time [$level] $message\n"
end
