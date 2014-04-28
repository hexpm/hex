defmodule Hex.Mixfile do
  use Mix.Project

  def project do
    [ app: :hex,
      version: "0.1.3-dev",
      elixir: "~> 0.13.1-dev",
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    [{ :hex_web, path: "../hex_web", only: :test, env: :test }]
  end
end
