#!/usr/bin/env bash

set -e -u

# $1   = rebar name
# $2   = rebar version
# $... = elixir version
function rebar_csv {
  for elixir in "${@:3}"
  do
    sha=$(shasum -a 512 "${1}")
    sha=($sha)
    echo "${2},${sha},${elixir}" >> ${1}-1.x.csv
  done

  openssl dgst -sha512 -sign "${ELIXIR_PEM}" "${1}-1.x.csv" | openssl base64 > ${1}-1.x.csv.signed
}

# $1 = source
# $2 = target
function s3cp {
  aws s3 cp "${1}" "s3://s3.hex.pm/installs/${2}" --acl public-read --cache-control "public, max-age=604800" --metadata "surrogate-key=installs"
}

# $1   = rebar name
# $2   = rebar version
# $... = elixir versions
function upload {
  for elixir in ${@:3}
  do
    s3cp "${1}" "${elixir}/${1}"
    s3cp "${1}" "${elixir}/${1}-${2}"
  done

  # special case 1.0.0 upload

  s3cp "${1}-1.x.csv" "${1}-1.x.csv"
  s3cp "${1}-1.x.csv.signed" "${1}-1.x.csv.signed"
}

# UPDATE THIS FOR EVERY RELEASE
# AND MAKE SURE WE HAVE rebar(3)-1.x.csv in the local directory

rebar_name=$1 # rebar / rebar3
rebar_version=$2

rebar_csv "${rebar_name}" "${rebar_version}" 1.0.0
upload    "${rebar_name}" "${rebar_version}" 1.0.0
