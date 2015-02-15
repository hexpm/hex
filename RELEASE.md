# Release process

This document simply outlines the release process:

1. Ensure you are running on the oldest supported Erlang version

2. Run `mix do clean, test` to ensure all tests pass from scratch and the CI is green

3. Remove all `-dev` extension from versions (see below for all files)

4. Ensure CHANGELOG is updated and add current date

5. Commit changes above with title "Release vVERSION" and generate new tag

6. Push master and the new tag

7. Build Hex with `MIX_ENV=prod mix archive.build` against supported Elixir versions (see below)

8. Build Hex with `MIX_ENV=prod mix archive.build -o hex.ez` giving the correct name in `-o` is important, renaming the file afterwards doesn't work

9. Upload builds to S3 (see below for paths)

10. Add new release by running `mix run scripts/add_install.exs HEX_VERSION ELIXIR_VERSION [, ELIXIR_VERSION]`

11. Increment version and add `-dev` extension to versions (see below for all files)

12. Commit changes above with title "Bump to vVERSION-dev"

13. Push master

## All builds

Hex needs to built for every Elixir supported vMAJOR.MINOR version. Currently that is every minor version released after 1.0.0.

Always build on the latest patch version and make sure tests pass before building the archive.

## S3 paths

* s3.hex.pm/installs/hex.ez (latest Hex built against oldest supported Elixir)
* s3.hex.pm/installs/[ELIXIR VERSION]/hex.ez
* s3.hex.pm/installs/[ELIXIR VERSION]/hex-[HEX VERSION].ez

## Places where version is mentioned

* mix.exs `:version` option
* CHANGELOG.md
