defmodule HasHexDep.Fixture.Mixfile do
  use Mix.Project

  def project do
    [app: :has_hex_dep,
      version: "0.0.1",
      deps: deps()]
  end

  defp deps do
    [{:ecto, []}]
  end
end
