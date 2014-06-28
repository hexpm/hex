defmodule Hex.Mixfile do
  use Mix.Project

  def project do
    [ app: :hex,
      version: "0.3.1-dev",
      elixir: "~> 0.14.2-dev",
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    [{:hex_web, github: "ericmj/hex_web", only: :test, env: :test}]
  end
end
