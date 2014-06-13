use Mix.Config

log_level = :notice

config :lager,
  handlers: [
    lager_console_backend:
      [log_level, {:lager_default_formatter, [:time, ' [', :severity, '] ', :message, '\n']}]
  ],
  crash_log: :undefined,
  error_logger_hwm: 150

config :stout,
  truncation_size: 4096,
  level: log_level
