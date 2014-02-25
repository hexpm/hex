# Installs a dev version of Hex locally
# Run with mix run local_install.exs

Mix.Task.run "archive", ["-o", "hex.ez"]
Mix.Task.run "local.install", ["hex.ez", "--force"]
