defmodule Hex.Mixfile do
  use Mix.Project

  def project do
    [ app: :hex,
      version: "0.0.1-dev",
      elixir: "~> 0.12.4 or ~> 0.13.0-dev",
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    [{ :hex_web, github: "ericmj/hex_web", only: :test }]
  end
end
