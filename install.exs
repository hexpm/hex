# Installs a dev version of Hex locally
# Run with mix run install.exs

Mix.Task.run "archive.build", ["-o", "hex.ez"]
Mix.Task.run "archive.install", ["hex.ez", "--force"]
