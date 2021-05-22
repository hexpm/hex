defmodule Sample.MixProject do
  def project do
    [
      app: :sample,
      description: "sample",
      version: "0.0.1",
      deps: [],
      package: [
        licenses: ["MIT"],
        files: ["myfile.txt", "mix.exs"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseSimple.MixProject do
  def project do
    [
      app: :release_a,
      description: "baz",
      version: "0.0.1",
      package: [
        licenses: ["MIT"],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseNewSimple.MixProject do
  def project do
    [
      app: :release_a_new,
      description: "baz",
      version: "0.0.1",
      package: [
        licenses: ["MIT"],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

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

defmodule ReleaseCustomRepoDeps.MixProject do
  def project do
    [
      app: :release_b_custom,
      description: "bar",
      version: "0.0.2",
      deps: [
        {:ex_doc, "0.0.1"},
        {:ecto, "0.0.1", repo: :my_repo}
      ],
      package: [
        licenses: ["MIT"]
      ]
    ]
  end
end

defmodule ReleaseMeta.MixProject do
  def project do
    [
      app: :release_c,
      version: "0.0.3",
      description: "foo",
      package: [
        files: ["myfile.txt", "missing.txt", "missing/*"],
        licenses: ["Apache"],
        links: %{"a" => "http://a"},
        extra: %{"c" => "d"}
      ]
    ]
  end
end

defmodule ReleaseMetaNoFiles.MixProject do
  def project do
    [
      app: :release_nf,
      version: "0.0.3",
      description: "foo",
      package: [
        files: [],
        licenses: ["Apache"],
        links: %{"a" => "http://a"},
        extra: %{"c" => "d"}
      ]
    ]
  end
end

defmodule ReleaseName.MixProject do
  def project do
    [
      app: :release_d,
      description: "Whatever",
      version: "0.0.1",
      package: [
        name: :released_name,
        licenses: ["MIT"],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseNoDescription.MixProject do
  def project do
    [
      app: :release_e,
      version: "0.0.1"
    ]
  end
end

defmodule ReleaseTooLongDescription.MixProject do
  def project do
    [
      app: :release_f,
      description: String.duplicate("w", 301),
      version: "0.0.1"
    ]
  end
end

defmodule ReleasePreDeps.MixProject do
  def project do
    [
      app: :release_g,
      description: "bar",
      version: "0.0.1",
      deps: [
        {:ex_doc, "~> 0.0.1-pre"}
      ],
      package: [
        files: ["myfile.txt"],
        licenses: ["MIT"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseFiles.MixProject do
  def project do
    [
      app: :release_h,
      version: "0.0.1",
      description: "foo",
      package: [
        files: ["myfile.txt", "executable.sh", "dir", "empty_dir", "link_dir"],
        licenses: ["MIT"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseExcludePatterns.MixProject do
  def project do
    [
      app: :release_i,
      version: "0.0.1",
      description: "foo",
      package: [
        files: ["myfile.txt"],
        exclude_patterns: ["exclude.*"],
        licenses: ["MIT"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseRepo.MixProject do
  def project do
    [
      app: :ecto,
      description: "baz",
      version: "0.0.1",
      package: [
        organization: "myorg",
        licenses: ["MIT"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseRepoInvalidLicenses.MixProject do
  def project do
    [
      app: :release_repo_invalid_licenses,
      description: "Invalid license",
      version: "0.0.1",
      package: [
        organization: "myorg",
        licenses: ["CustomLicense"],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
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
        files: ["myfile.txt"],
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
      deps: [
        {:ex_doc, "0.0.1"}
      ],
      package: [
        licenses: ["MIT"],
        links: %{"a" => "http://repo"}
      ]
    ]
  end
end

defmodule ReleaseGitDeps.MixProject do
  def project do
    [
      app: :release_git_deps,
      description: "Package with git dependencies",
      version: "0.0.2",
      deps: [
        {:ex_doc, "0.0.1"},
        {:ecto, github: "elixir-ecto/ecto", tag: "v0.2.5"},
        {:gettext, git: "https://github.com/elixir-lang/gettext.git", branch: "master"}
      ],
      package: [
        licenses: ["MIT"],
        links: %{"example" => "http://example.com"}
      ]
    ]
  end
end

defmodule ReleaseCustomApiUrl.MixProject do
  def project do
    [
      app: :release_custom_api_url,
      description: "Package with custom api_url",
      version: "0.0.1",
      deps: [],
      package: [
        licenses: ["MIT"],
        links: %{"a" => "http://a"}
      ],
      hex: [
        api_url: "https://custom"
      ]
    ]
  end
end

defmodule ReleaseMissingLicenses.MixProject do
  def project do
    [
      app: :release_missing_licenses,
      description: "Package with missing licenses",
      version: "0.0.1",
      package: [
        licenses: [],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseInvalidLicenses.MixProject do
  def project do
    [
      app: :release_invalid_licenses,
      description: "Package with invalid licenses",
      version: "0.0.1",
      package: [
        licenses: ["CustomLicense"],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end
