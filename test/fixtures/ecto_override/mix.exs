defmodule EctoOverride.Fixture.MixProject do
  use Mix.Project

  def project do
    [app: :ecto, version: "0.2.1", deps: deps()]
  end

  defp deps do
    [{:postgrex, "0.2.0", override: true}]
  end
end
