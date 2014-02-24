# Hex

[![Build Status](https://travis-ci.org/ericmj/hex.png?branch=master "Build Status")](http://travis-ci.org/ericmj/hex)

Hex is package manager for [Elixir](https://github.com/elixir-lang/elixir) that integrates with Mix.

## Installation

Install by running in your shell `mix local.install https://hex.pm/archives/hex.ez && mix hex.update`. The Hex installation can later be updated with `mix hex.update --system`.

## Usage

Hex integrates with Mix's dependency handling. This means that it utilizes Mix's dependency lock `mix.lock`, dependencies are defined in Mix's format and all the ordinary Mix dependency commands work. A Hex package dependency is defined with `package: true`. `Code.ensure_loaded?(Hex) && Hex.start` is required at the top of the mixfile to ensure that Hex integrates with Mix.

Below is an example mix.exs file.

```elixir
Code.ensure_loaded?(Hex) && Hex.start

defmodule MyProject.Mixfile do
  use Mix.Project

  def project do
    [ app: :my_project,
      version: "0.0.1",
      elixir: "~> 0.12.4",
      deps: deps() ]
  end

  def application, do: []

  defp deps do
    [ { :ecto, "~> 0.1.0", package: true },
      { :postgrex, "~> 0.3.0", package: true },
      { :cowboy, github: "extend/cowboy" } ]
  end
end
```

Dependencies are fetched and updated with the normal Mix deps commands: `mix deps.get`, `mix deps.update` etc.

When `mix deps.get` is called when you have unlocked dependencies Hex will perform dependency resolution. The dependency resolver finds package releases for your dependencies that are guaranteed to be compatible with all other dependencies. The dependency resolver will always try to use the latest release of any package.

Because of the nature of dependency resolution Hex may sometimes fail to find compatible releases of all your dependencies. This can be resolved by unlocking dependencies with `mix deps.unlock`, more unlocked dependencies gives Hex a larger selection of releases to work with. It may also be the case that there is no way to resolve a conflict between your dependencies, then you need to contact the package maintainers so they can update their package requirements.

Hex supplies a few extra Mix tasks. They are prefixed with `hex` and can be found with `mix help`.

## Browsing for packages

Packages can be searched for with `mix hex.search`. Additional information about a package can be found with `mix hex.info`.

## Creating a package and release

See `mix help hex.release`

## Roadmap and TODO

* Website.

* Honour `override: true` during resolution.

* Package maintainers that can do releases.

* Package into tarballs that we host instead of using git urls and refs.

* escript client for erlang users (should not require an elixir installation).

* Use elixir version requirement during resolution (wait for elixir 1.0.0).

## Contributing

Integration testing against the API server requires a postgresql user with username `postgres` and password `postgres`.

Run integration tests with `mix test --include integration`.

Also see the API server repository: [hex_web](https://github.com/ericmj/hex_web).
