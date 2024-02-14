#!/usr/bin/env bash

set -e -u

function join { local IFS="$1"; shift; echo "$*"; }

# $1 = hex version
# $2 = erlang version
# $3 = elixir version
# $4 = saved elixir version
# $5 = ubuntu version
function build {
  rm -rf _build src/mix_safe_erl_term.erl
  docker run -v $PWD:/hex hexpm/elixir:${3}-erlang-${2}-ubuntu-${5} sh -c "mix local.hex --force"
  docker run -v $PWD:/hex hexpm/elixir:${3}-erlang-${2}-ubuntu-${5} sh -c "cd /hex && mix local.hex --force && MIX_ENV=prod mix archive.build -o hex.ez"
  docker run -v $PWD:/hex hexpm/elixir:${3}-erlang-${2}-ubuntu-${5} sh -c "cd /hex && mix local.hex --force && MIX_ENV=prod mix archive.build -o hex-${1}.ez"
  cp hex.ez hex-elixir-${4}.ez
  cp hex-${1}.ez hex-${1}-elixir-${4}.ez
}

# $1   = hex version
# $... = elixir version
function hex_csv {
  rm hex-1.x*.csv || true
  s3down hex-1.x.csv hex-1.x.csv

  for elixir in "${@:2}"
  do
    sha=$(shasum -a 512 hex-${1}-elixir-${elixir}.ez)
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
    s3up "hex-elixir-${elixir}.ez" "${elixir}/hex.ez"
    s3up "hex-${1}-elixir-${elixir}.ez" "${elixir}/hex-${1}.ez"
  done

  s3up hex-1.x.csv hex-1.x.csv
  s3up hex-1.x.csv.signed hex-1.x.csv.signed
}

hex_version=$1

# UPDATE THIS FOR EVERY RELEASE
build ${hex_version} 24.3.4.16 1.16.1 1.16.0 focal-20240123
build ${hex_version} 24.3.4.16 1.15.7 1.15.0 focal-20240123
build ${hex_version} 23.3 1.14.2 1.14.0 xenial-20210114
build ${hex_version} 22.3 1.13.4 1.13.0 xenial-20200212
build ${hex_version} 22.3 1.12.3 1.12.0 xenial-20200212
build ${hex_version} 21.3 1.11.4 1.11.0 xenial-20200212
build ${hex_version} 21.3 1.10.4 1.10.0 xenial-20200212
build ${hex_version} 20.3 1.9.4 1.9.0 xenial-20200212
build ${hex_version} 20.3 1.8.2 1.8.0 xenial-20200212
build ${hex_version} 19.3 1.7.4 1.7.0 xenial-20200212
build ${hex_version} 19.3 1.6.6 1.6.0 xenial-20200212
build ${hex_version} 18.3 1.5.3 1.5.0 xenial-20200212
rm -rf _build

hex_csv "${hex_version}" 1.5.0 1.6.0 1.7.0 1.8.0 1.9.0 1.10.0 1.11.0 1.12.0 1.13.0 1.14.0 1.15.0 1.16.0
upload  "${hex_version}" 1.5.0 1.6.0 1.7.0 1.8.0 1.9.0 1.10.0 1.11.0 1.12.0 1.13.0 1.14.0 1.15.0 1.16.0
