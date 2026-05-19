import Config

if config_env() == :test do
  config :logger, level: :warning
  config :logger, :console, format: "$date $time [$level] $message\n"
end
