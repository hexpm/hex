defmodule Umbrella.MyApp2.Fixture.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app2,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      deps: deps()
    ]
  end

  defp deps do
    [{:my_app1, in_umbrella: true}]
  end
end
