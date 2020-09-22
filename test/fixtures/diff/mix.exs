defmodule ReleaseDeps.MixProject do
  def project do
    [
      app: :release_b,
      description: "bar",
      version: "0.0.2",
      deps: [
        {:ex_doc, "0.0.1"}
      ],
      package: [
        licenses: ["MIT"]
      ]
    ]
  end
end
