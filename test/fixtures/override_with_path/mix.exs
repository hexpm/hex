defmodule OverrideWithPath.Fixture.Mixfile do
  use Mix.Project

  def project do
    [ app: :override_with_git,
      version: "0.1.0",
      deps: [ {:postgrex, []},
              {:ex_doc, path: "../ex_doc", override: true}] ]
  end
end
