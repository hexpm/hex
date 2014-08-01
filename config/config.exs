use Mix.Config

if Mix.env == :test do
  config :hex_web,
    password_work_factor: 4,
    url: "http://localhost:4000"

  config :logger,
    level: :warn

  config :logger, :console,
    format: "$date $time [$level] $message\n"
end
