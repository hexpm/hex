#!/usr/bin/env bash

# Build and publish Hex's own documentation to https://hexdocs.pm/hex.
#
# Hex is distributed as an archive rather than a registry package, so its docs
# can't be published with `mix hex.publish docs`. Instead they are built with
# ExDoc and the tarball is uploaded to the docs store, exactly as
# elixir-lang/elixir's release workflow publishes the standard-library docs.
#
# Usage:
#
#     $ ./scripts/release_docs.sh HEX_VERSION
#
# Pass --dry-run to build the docs tarball without uploading:
#
#     $ ./scripts/release_docs.sh --dry-run HEX_VERSION
#
# Requires docker, aws, and the fastly CLI, with their credentials configured,
# as for release_hex.sh.

set -e -u -o pipefail

# Toolchain used to build the docs. Keep roughly in step with release_hex.sh.
elixir_image="hexpm/elixir:1.18.4-erlang-27.3.4.1-ubuntu-jammy-20250530"
ex_doc_version="0.40.3"

# Oldest Hex version listed in the docs version dropdown.
docs_min_version="0.17.0"

# Bucket fronted by the repo Fastly service; docs live under its docs/ prefix.
s3_bucket="${HEX_AWS_S3_BUCKET:-s3.hex.pm}"

# Fastly service for repo.hex.pm, which serves the docs/ tarballs.
fastly_repo_service="hiohGRsDbo44nWc8jzdHaH"

function main {
  dry_run=false
  local args=()
  for arg in "$@"; do
    if [ "$arg" = "--dry-run" ]; then
      dry_run=true
    else
      args+=("$arg")
    fi
  done

  if [ ${#args[@]} -ne 1 ]; then
    echo "usage: release_docs.sh [--dry-run] HEX_VERSION" >&2
    exit 1
  fi

  local version="${args[0]#v}"
  local ref="v${version}"
  local tarball="hex-${version}.tar.gz"
  local surrogate_key="docs/hex-${version}"

  build_docs "${ref}" "${version}"
  tar -czf "${tarball}" -C doc .

  if [ "${dry_run}" = true ]; then
    echo "Dry run complete: ${tarball}"
    exit 0
  fi

  aws s3 cp "${tarball}" "s3://${s3_bucket}/docs/${tarball}" \
    --cache-control "public,max-age=3600" \
    --metadata "{\"surrogate-key\":\"${surrogate_key}\",\"surrogate-control\":\"public,max-age=604800\"}"

  purge "${surrogate_key}"

  echo "Published https://hexdocs.pm/hex/${version}"
}

# Build hex's docs for $ref into ./doc using ExDoc inside a pinned container.
# The release is cloned fresh at its tag (with full history so all tags are
# available for the version dropdown) so the docs match the published code and
# the build never touches the working tree.
#
# A docs_config.js listing every release is written into the docs: HexDocs reads
# it from the tarball for unmanaged packages like hex instead of generating one.
function build_docs {
  local ref="$1" version="$2"
  rm -rf doc

  docker run --rm -v "$(pwd)":/out "${elixir_image}" bash -euxc "
    apt-get update && apt-get install -y git
    mix local.hex --force
    mix local.rebar --force
    mix escript.install --force hex ex_doc ${ex_doc_version}

    git clone --quiet --branch ${ref} https://github.com/hexpm/hex.git /hex
    cd /hex
    mix deps.get
    mix compile
    \"\$HOME/.mix/escripts/ex_doc\" Hex ${version} _build/dev/lib/hex/ebin \
      -m Mix.Tasks.Hex \
      -u https://github.com/hexpm/hex \
      --source-ref ${ref} \
      --logo /out/scripts/hex_logo.png

    git tag | elixir /out/scripts/docs_config.exs hex ${docs_min_version} > doc/docs_config.js

    rm -rf /out/doc
    cp -R doc /out/doc
  "
}

# Purge the docs tarball from the repo Fastly service so repo.hex.pm/docs serves
# the new build; HexDocs purges its own serving cache after ingesting it. Run
# twice with a pause to beat any in-flight requests, like release_hex.sh.
function purge {
  local key="$1"
  fastly service purge --service-id "${fastly_repo_service}" --key "${key}"
  sleep 5
  fastly service purge --service-id "${fastly_repo_service}" --key "${key}"
}

main "$@"
