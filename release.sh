#!/usr/bin/env bash

set -e -u

function join { local IFS="$1"; shift; echo "$*"; }

# $1 = hex version
# $2 = erlang version
# $3 = elixir version
# $4 = saved elixir version
function build {
  rm .tool-versions || true
  rm -rf _build || true
  rm src/safe_erl_term.erl || true

  printf "erlang ${2}\nelixir ${3}" > .tool-versions

  MIX_ENV=prod mix compile

  MIX_ENV=prod mix archive.build
  MIX_ENV=prod mix archive.build -o hex.ez

  mv hex.ez "hex-${4}.ez"
  mv "hex-${1}.ez" "hex-${1}-${4}.ez"
}

# $1   = hex version
# $... = elixir version
function hex_csv {
  rm hex-1.x*.csv || true

  s3down hex-1.x.csv hex-1.x.csv

  for elixir in "${@:2}"
  do
    sha=$(shasum -a 512 hex-${1}-${elixir}.ez)
    sha=($sha)
    echo "${1},${sha},${elixir}" >> hex-1.x.csv
  done

  openssl dgst -sha512 -sign "${ELIXIR_PEM}" hex-1.x.csv | openssl base64 > hex-1.x.csv.signed
}

# $1 = source
# $2 = target
function s3up {
  aws s3 cp "${1}" "s3://s3.hex.pm/installs/${2}" --acl public-read --cache-control "public, max-age=604800" --metadata "surrogate-key=installs"
}

# $1 = source
# $2 = target
function s3down {
  aws s3 cp "s3://s3.hex.pm/installs/${1}" "${2}"
}

# $1   = hex version
# $... = elixir versions
function upload {
  for elixir in "${@:2}"
  do
    s3up "hex-${elixir}.ez" "${elixir}/hex.ez"
    s3up "hex-${1}-${elixir}.ez" "${elixir}/hex-${1}.ez"
  done

  # special case 1.0.0 upload
  s3up hex-1.0.0.ez hex.ez

  s3up hex-1.x.csv hex-1.x.csv
  s3up hex-1.x.csv.signed hex-1.x.csv.signed
}


# UPDATE THIS FOR EVERY RELEASE
hex_version=$1

build ${hex_version} 18.3.4.4 1.3.4 1.3.0
build ${hex_version} 18.3.4.4 1.2.6 1.2.0
build ${hex_version} 17.5.6.9 1.1.1 1.1.0
build ${hex_version} 17.5.6.9 1.0.5 1.0.0

hex_csv "${hex_version}" 1.0.0 1.1.0 1.2.0 1.3.0
upload  "${hex_version}" 1.0.0 1.1.0 1.2.0 1.3.0


rm -rf _build
rm -rf .tool-versions
