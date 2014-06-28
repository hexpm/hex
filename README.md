# Hex

[![Build Status](https://travis-ci.org/ericmj/hex.png?branch=master "Build Status")](http://travis-ci.org/ericmj/hex)

Hex is package manager for the Erlang VM.

This project currently provides tasks that integrate with Mix, [Elixir](https://github.com/elixir-lang/elixir)'s build tool.

## Contributing

Integration testing against the API server requires a postgresql user with username `postgres` and password `postgres`.

Run integration tests with `mix test --include integration`.

Also see the API server repository: [hex_web](https://github.com/ericmj/hex_web).
