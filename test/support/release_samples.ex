defmodule ReleaseSimple.MixProject do
  def project do
    [app: :release_a,
     description: "baz",
     version: "0.0.1",
     package: [licenses: ["MIT"],
      maintainers: ["maintainers"],
      links: %{"a" => "http://a"}]]
  end
end

defmodule ReleaseDeps.MixProject do
  def project do
    [app: :release_b, description: "bar", version: "0.0.2",
     deps: [{:ex_doc, "0.0.1"}],
     package: [licenses: ["MIT"]]]
  end
end

defmodule ReleaseCustomRepoDeps.MixProject do
  def project do
    [
      app: :release_b_custom, description: "bar", version: "0.0.2",
      deps: [
        {:ex_doc, "0.0.1"},
        {:ecto, "0.0.1", repo: :my_repo},
      ],
      package: [licenses: ["MIT"]]
    ]
  end
end

defmodule ReleaseMeta.MixProject do
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

defmodule ReleaseName.MixProject do
  def project do
    [app: :release_d, description: "Whatever", version: "0.0.1",
     package: [name: :released_name,
       licenses: ["MIT"],
       maintainers: ["maintainers"],
       links: %{"a" => "http://a"}]]
  end
end

defmodule ReleaseNoDescription.MixProject do
  def project do
    [app: :release_e, version: "0.0.1"]
  end
end

defmodule ReleaseTooLongDescription.MixProject do
  def project do
    [app: :release_f, description: String.duplicate("w", 301),
     version: "0.0.1"]
  end
end

defmodule ReleasePreDeps.MixProject do
  def project do
    [app: :release_g, description: "bar", version: "0.0.1",
     deps: [{:ex_doc, "~> 0.0.1-pre"}],
     package: [files: ["myfile.txt"],
       licenses: ["MIT"],
       links: %{"a" => "http://a"},
       maintainers: ["maintainers"]]]
  end
end

defmodule ReleaseFiles.MixProject do
  def project do
    [app: :release_h,
     version: "0.0.1",
     description: "foo",
     package: [
       files: ["myfile.txt", "executable.sh"],
       licenses: ["MIT"],
       links: %{"a" => "http://a"},
       maintainers: ["maintainers"]]]
  end
end

defmodule ReleaseRepo.MixProject do
  def project do
    [app: :ecto,
     description: "baz",
     version: "0.0.1",
     package: [
       organization: "myorg",
       licenses: ["MIT"],
       maintainers: ["maintainers"],
       links: %{"a" => "http://a"}]]
  end
end

defmodule ReleaseMisspelledOrganization.MixProject do
  def project do
    [
      app: :release_a,
      description: "baz",
      version: "0.0.1",
      package: [
        organisation: "myorg",
        licenses: ["MIT"],
        maintainers: ["maintainers"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseOrganizationWrongLocation.MixProject do
  def project do
    [
      app: :ecto,
      description: "baz",
      version: "0.0.1",
      organization: "myorg",
      package: [
        licenses: ["MIT"],
        maintainers: ["maintainers"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseIncludeReservedFile.MixProject do
  def project do
    [
      app: :release_a,
      description: "baz",
      version: "0.0.1",
      package: [
        licenses: ["MIT"],
        maintainers: ["maintainers"],
        links: %{"a" => "http://a"},
        files: ["hex_meta*"]
      ]
    ]
  end
end

defmodule ReleaseIncludeRepoDeps.MixProject do
  def project do
    [
      app: :release_a,
      description: "baz",
      version: "0.0.1",
      package: [
        licenses: ["MIT"],
        maintainers: ["maintainers"],
        links: %{"a" => "http://repo"},
      ],
      deps: [
        {:ex_doc, "0.0.1"},
      ]
    ]
  end
end
