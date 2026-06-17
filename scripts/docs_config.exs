#!/usr/bin/env elixir
#
# Usage: git tag | docs_config.exs PROJECT [MIN_VERSION]
#
# Reads version tags on stdin and prints the docs_config.js that HexDocs serves
# to populate the version dropdown. For unmanaged packages such as hex, HexDocs
# uses the docs_config.js bundled in the docs tarball rather than generating one,
# so this file must be included at the root of the tarball.

[project | rest] = System.argv()
min_version = List.first(rest) || "0.0.0"

nodes =
  IO.read(:stdio, :eof)
  |> String.split(~r/\s+/, trim: true)
  |> Enum.filter(&String.starts_with?(&1, "v"))
  |> Enum.map(&String.trim_leading(&1, "v"))
  |> Enum.filter(&match?({:ok, _}, Version.parse(&1)))
  |> Enum.filter(&(Version.compare(&1, min_version) in [:gt, :eq]))
  |> Enum.sort({:desc, Version})
  |> Enum.map_join(", ", fn version ->
    ~s({"version":"v#{version}", "url":"https://#{String.replace(project, "_", "-")}.hexdocs.pm/#{version}"})
  end)

IO.puts("var versionNodes = [#{nodes}];")
