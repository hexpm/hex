import Config

if config_env() == :test do
  config :logger, level: :warn
  config :logger, :console, format: "$date $time [$level] $message\n"
end
