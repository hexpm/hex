# Release process

This document simply outlines the release process:

1. Ensure you are running on the oldest supported Erlang version

1. Remove all `-dev` extension from versions (see below for all files)

2. Ensure CHANGELOG is updated and add current date

3. Commit changes above with title "Release vVERSION" and generate new tag

4. Push master and the new tag

5. Run `make clean test` to ensure all tests pass from scratch and the CI is green

6. Build Hex with `MIX_ENV=prod mix archive.build` against supported Elixir versions (see below)

7. Upload builds to S3 (see below for paths)

8. Add new release to `installs` table in the hex.pm database (see below for SQL)

9. Rebuild the registry file by running `HexWeb.RegistryBuilder.sync_rebuild` on the hex.pm server

9. Increment version and add `-dev` extension to versions (see below for all files)

10. Commit changes above with title "Bump to vVERSION-dev"

11. Push master

## All builds

Hex needs to built for every Elixir supported vMAJOR.MINOR version. Currently that is every minor version released after 1.0.0.

## S3 paths

* s3.hex.pm/installs/hex.ez (latest Hex built against oldest supported Elixir)
* s3.hex.pm/installs/[ELIXIR VERSION]/hex-[HEX VERSION].ez

## SQL script

    INSERT INTO installs (hex, elixirs) VALUES ('0.6.0', ARRAY['1.0.0']);

The first value is the released Hex version the second value is all Elixir supported vMAJOR.MINOR version.

## Places where version is mentioned

* mix.exs `:version` option
* CHANGELOG.md
