use Mix.Config

if Mix.env() == :test do
  config :logger, level: :warn

  config :logger, :console, format: "$date $time [$level] $message\n"
end
