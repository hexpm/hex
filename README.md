# Hex

[![Build Status](https://travis-ci.org/ericmj/hex.png?branch=master "Build Status")](http://travis-ci.org/ericmj/hex)

Hex is package manager for [Elixir](https://github.com/elixir-lang/elixir) that integrates with Mix.

## Roadmap and TODO

* Package maintainers that can do releases

* escript client for erlang users (should not require an elixir installation)

* Use elixir version requirement during resolution (wait for elixir 1.0.0)

## Contributing

Integration testing against the API server requires a postgresql user with username `postgres` and password `postgres`.

Run integration tests with `mix test --include integration`.

Also see the API server repository: [hex_web](https://github.com/ericmj/hex_web).
