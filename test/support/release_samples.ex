defmodule Sample.MixProject do
  def project do
    [
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name, :release_deps),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
      version: "0.0.3",
      description: "foo",
      package: [
        files: ["myfile.txt", "missing.txt", "missing/*"],
        licenses: ["Apache-2.0"],
        links: %{"a" => "http://a"},
        extra: %{"c" => %{"d" => "e"}}
      ]
    ]
  end
end

defmodule ReleaseMetaNoFiles.MixProject do
  def project do
    [
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
      version: "0.0.3",
      description: "foo",
      package: [
        files: [],
        licenses: ["Apache-2.0"],
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
        name: Process.get(:hex_test_package_name) || raise("missing package name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
      version: "0.0.1"
    ]
  end
end

defmodule ReleaseTooLongDescription.MixProject do
  def project do
    [
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
      description: String.duplicate("w", 301),
      version: "0.0.1"
    ]
  end
end

defmodule ReleasePreDeps.MixProject do
  def project do
    [
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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

defmodule ReleaseGitDeps.MixProject do
  def project do
    [
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
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

defmodule ReleaseAppFalseDep.MixProject do
  def project do
    [
      app: :release_app_false_dep,
      description: "Package with invalid licenses",
      version: "0.0.1",
      deps: [
        {:ex_doc, "0.0.1", app: false}
      ],
      package: [
        licenses: [],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end

defmodule ReleaseInUmbrellaDeps.MixProject do
  def project do
    [
      app: Process.get(:hex_test_app_name) || raise("missing app name"),
      description: "includes umbrella",
      version: "0.2.1",
      deps: [
        {:ecto, "3.3.2", in_umbrella: true},
        {:postgrex, "0.2.1"}
      ],
      package: [
        licenses: ["MIT"],
        files: ["myfile.txt"],
        links: %{"a" => "http://a"}
      ]
    ]
  end
end
