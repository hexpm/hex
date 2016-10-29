defmodule ReleaseSimple.Mixfile do
  def project do
    [app: :release_a,
     description: "baz",
     version: "0.0.1",
     package: [licenses: ["MIT"],
      maintainers: ["maintainers"],
      links: %{"a" => "http://a"}]]
  end
end

defmodule ReleaseDeps.Mixfile do
  def project do
    [app: :release_b, description: "bar", version: "0.0.2",
     deps: [{:ex_doc, "0.0.1"}],
     package: [licenses: ["MIT"]]]
  end
end

defmodule ReleaseMeta.Mixfile do
  def project do
    [app: :release_c, version: "0.0.3",
     description: "foo",
     package: [files: ["myfile.txt", "missing.txt", "missing/*"],
       licenses: ["Apache"],
       links: %{"a" => "http://a"},
       maintainers: ["maintainers"],
       extra: %{"c" => "d"}]]
  end
end

defmodule ReleaseName.Mixfile do
  def project do
    [app: :release_d, description: "Whatever", version: "0.0.1",
     package: [name: :released_name,
       licenses: ["MIT"],
       maintainers: ["maintainers"],
       links: %{"a" => "http://a"}]]
  end
end

defmodule ReleaseNoDescription.Mixfile do
  def project do
    [app: :release_e, version: "0.0.1"]
  end
end

defmodule ReleaseTooLongDescription.Mixfile do
  def project do
    [app: :release_f, description: String.duplicate("w", 301),
     version: "0.0.1"]
  end
end
