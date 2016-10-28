# Release process

This document simply outlines the release process:

1. Ensure you are running on the oldest supported Erlang version

2. Run `mix do clean --deps, clean && mix test --include integration` to ensure all tests pass from scratch and the CI is green

3. Remove all `-dev` extension from versions (see below for all files)

4. Ensure CHANGELOG is updated and add current date

5. Commit changes above with title "Release vVERSION" and generate new tag

6. Push master and the new tag

7. Build Hex with `MIX_ENV=prod mix archive.build` against supported Elixir versions (see below)

8. Build Hex with `MIX_ENV=prod mix archive.build -o hex.ez` giving the correct name in `-o` is important, renaming the file afterwards doesn't work

9. Update hex release csv, sign and upload to S3 (see below for instructions)

10. Upload builds, csv and signed files to S3 (see below for paths)

11. Add new release by running `mix run scripts/add_install.exs HEX_VERSION ELIXIR_VERSION [, ELIXIR_VERSION]`

12. Increment version and add `-dev` extension to versions (see below for all files)

13. Commit changes above with title "Bump to vVERSION-dev"

14. Push master

## All builds

Hex needs to built for every Elixir supported vMAJOR.MINOR version. Currently that is every minor version released after 1.0.0.

Always build on the latest patch version and make sure tests pass before building the archive.

## Places where version is mentioned

* mix.exs `@version` attribute
* CHANGELOG.md

## S3 paths

* s3.hex.pm/installs/hex.ez (latest Hex built against oldest supported Elixir)
* s3.hex.pm/installs/[ELIXIR]/hex.ez
* s3.hex.pm/installs/[ELIXIR]/hex-[HEX].ez
* s3.hex.pm/installs/hex-1.x.csv
* s3.hex.pm/installs/hex-1.x.csv.signed

## S3 upload commands

Only for oldest elixir and OTP version:

```
aws s3 cp hex.ez s3://s3.hex.pm/installs/hex.ez --acl public-read
```

```
aws s3 cp hex.ez             s3://s3.hex.pm/installs/[ELIXIR]/hex.ez       --acl public-read &&
aws s3 cp hex-[HEX].ez       s3://s3.hex.pm/installs/[ELIXIR]/hex-[HEX].ez --acl public-read &&
aws s3 cp hex-1.x.csv        s3://s3.hex.pm/installs/hex-1.x.csv           --acl public-read &&
aws s3 cp hex-1.x.csv.signed s3://s3.hex.pm/installs/hex-1.x.csv.signed    --acl public-read
```

## Hex release CSV

### CSV format

```
hex_version,sha512(hex-[HEX].ez),elixir_version
```

Example:

```
0.9.0,4f6eae32124500117691740358df2a078d014f4d396a56a73b3e553e0b112b3f0ac9e0f7e0763feb85c889bac20571c6e028e5f4c252ac158cbb3c586efe992f,1.0.0
0.9.0,21fd3dbff18b2d2d51b41e147ac8bd13188a9840dae8f4ced6c150e227df64c3c6c5a472c3fd74e170f14fcf7cbeb7d85e12a520438bf0731c1ac55d2f6a4a8a,1.1.0
```

### Generate sha

```
shasum -a 512 hex-[HEX].ez
```

### Sign CSV

```
openssl dgst -sha512 -sign elixir.pem hex-1.x.csv | openssl base64 > hex-1.x.csv.signed
```
