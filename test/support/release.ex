defmodule ReleaseSimple.Mixfile do
  def project do
    [app: :releasea, description: "baz", version: "0.0.1"]
  end
end

defmodule ReleaseDeps.Mixfile do
  def project do
    [app: :releaseb, description: "bar", version: "0.0.2",
     deps: [{:ex_doc, "0.0.1"}]]
  end
end

defmodule ReleaseMeta.Mixfile do
  def project do
    [app: :releasec, version: "0.0.3",
     description: "foo",
     package: [files: ["myfile.txt", "missing.txt", "missing/*"],
       licenses: ["Apache"],
       links: %{"a" => "b"},
       maintainers: ["maintainers"]]]
  end
end

defmodule ReleaseName.Mixfile do
  def project do
    [app: :released, description: "Whatever", version: "0.0.1",
     package: [name: :released_name]]
  end
end

defmodule ReleaseNoDescription.Mixfile do
  def project do
    [app: :releasee, version: "0.0.1"]
  end
end
