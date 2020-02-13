defmodule Ecto.Enum_1_4_0.Fixture.MixProject do
  use Mix.Project

  def project do
    [app: :ecto_enum, version: "1.4.0", deps: deps()]
  end

  defp deps do
    [{:ecto, ">= 3.0.0"}]
  end
end
