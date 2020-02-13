defmodule Ecto.SQL_3_3_2.Fixture.MixProject do
  use Mix.Project

  def project do
    [app: :ecto_sql, version: "3.3.3", deps: deps()]
  end

  defp deps do
    [{:ecto, "~> 3.3.2"}]
  end
end
