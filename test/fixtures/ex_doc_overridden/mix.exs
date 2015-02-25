defmodule ExDocOverridden.Mixfile do
  use Mix.Project

  def project do
    [ app: :ex_doc_overridden,
      version: "0.1.0",
      deps: deps ]
  end

  defp deps do
    [{:ex_doc, "0.0.1", override: true}]
  end
end
