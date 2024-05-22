defmodule UmbrellaOverride.MyApp1.Fixture.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app1,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      deps: deps()
    ]
  end

  defp deps do
    [{:ecto_override, path: HexTest.Case.fixture_path("ecto_override")}]
  end
end
