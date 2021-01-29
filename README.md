# Hex

[![CI](https://github.com/hexpm/hex/workflows/CI/badge.svg)](https://github.com/hexpm/hex/actions)

Hex is package manager for the Erlang VM.

This project currently provides tasks that integrate with Mix, [Elixir](https://github.com/elixir-lang/elixir)'s build tool.

See [hex.pm](https://hex.pm) for installation instructions and other documentation.

## Contributing

Install Hex locally for development with: `mix install`.

### Bundled CA certs

Hex bundles a list of root CA certificates used for certificate validation in HTTPS. The certificates are fetched from [Mozilla's source tree](http://hg.mozilla.org/releases/mozilla-release/raw-file/default/security/nss/lib/ckfw/builtins/certdata.txt) with curl's [mk-ca-bundle.pl](https://github.com/bagder/curl/blob/master/lib/mk-ca-bundle.pl) script. The bundle created from the Perl script is stored in `lib/hex/http/ca-bundle.crt` and is included in source control, the file should be updated when new releases are made by Mozilla. When Hex is compiled the certificates are parsed and included with the compiled artifacts. The task `mix certdata` automates this process.

### hexpm

Integration tests run against the API server [hexpm](https://github.com/hexpm/hexpm). It needs to be cloned into `../hexpm` or `HEXPM_PATH` needs to be set and point its location. hexpm also requires postgresql with username `postgres` and password `postgres`.

Exclude integration tests with `mix test --exclude integration`.

## License

   Copyright 2015 Six Colors AB

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
