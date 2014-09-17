# Hex

[![Build Status](https://travis-ci.org/hexpm/hex.svg?branch=master "Build Status")](http://travis-ci.org/hexpm/hex)

Hex is package manager for the Erlang VM.

This project currently provides tasks that integrate with Mix, [Elixir](https://github.com/elixir-lang/elixir)'s build tool.

See [hex.pm](https://hex.pm) for installation instructions and other documentation.

## Contributing

Install hex locally for development with: `mix install`.

### Bundled CA certs

Hex bundles a list of root CA certificates used for certificate validation in HTTPS. The certificates are fetched from [Mozilla's source tree](http://hg.mozilla.org/releases/mozilla-release/raw-file/default/security/nss/lib/ckfw/builtins/certdata.txt) with curl's [mk-ca-bundle.pl](https://github.com/bagder/curl/blob/master/lib/mk-ca-bundle.pl) script. The bundle created from the Perl script is stored in `lib/hex/api/ca-bundle.crt` and is included in source control, the file should be updated when new releases are made by Mozilla. When Hex is compiled the certificates are parsed and included with the compiled artifacts.

### `hex_web`

Integration testing against the API server requires a postgresql user with username `postgres` and password `postgres`.

Run integration tests with `mix test --include integration`.

Also see the API server repository: [hex_web](https://github.com/hexpm/hex_web).
