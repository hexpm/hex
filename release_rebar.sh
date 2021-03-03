#!/usr/bin/env bash

set -e -u

# $1 = rebar name
# $2 = rebar version
# $3 = elixir version
function rebar_csv {
  s3down "${1}-1.x.csv" "${1}-1.x.csv"

  # Remove existing build for this elixir version
  csv=$(cat ${1}-1.x.csv)
  echo $csv | grep -v ",${3}\$" > ${1}-1.x.csv

  # Add new build
  sha=$(shasum -a 512 "${1}")
  sha=($sha)
  echo "${2},${sha},${3}" >> ${1}-1.x.csv

  openssl dgst -sha512 -sign "${ELIXIR_PEM}" "${1}-1.x.csv" | openssl base64 > ${1}-1.x.csv.signed
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

# $1 = rebar name
# $2 = rebar version
# $3 = elixir version
function upload {
  s3up "${1}" "${3}/${1}"
  s3up "${1}" "${3}/${1}-${2}"

  # Special case 1.0.0 upload
  s3up "${1}-1.x.csv" "${1}-1.x.csv"
  s3up "${1}-1.x.csv.signed" "${1}-1.x.csv.signed"
}

# $1 = rebar version
# $2 = otp version
function build_rebar2 {
  docker rm rebar2 || true

  docker run --name rebar2 hexpm/erlang:$-ubuntu-xenial-20201014 sh -c "\
    apt update && apt -y install git && \
    git clone https://github.com/rebar/rebar.git -b $1 && \
    cd rebar && \
    ./bootstrap"

  docker cp rebar2:/rebar/rebar rebar
  docker rm rebar2
}

# $1 = rebar version
# $2 = otp version
function build_rebar3 {
  docker rm rebar3 || true

  docker run --name rebar3 hexpm/erlang:$2-ubuntu-xenial-20201014 sh -c "\
    apt update && apt -y install git && \
    git clone https://github.com/erlang/rebar3.git -b $1 && \
    cd rebar3 && \
    ./bootstrap"

  docker cp rebar3:/rebar3/rebar3 rebar3
  docker rm rebar3
}

# Update these values for every release
# Call as ELIXIR_PEM=elixir.pem ./release_rebar.sh rebar3 3.14.4

rebar_name=$1 # rebar / rebar3
rebar_version=$2
elixir_version="1.11.4"
otp_version="21.3.8.21"

if [ "${rebar_name}" = "rebar" ]; then
  build_rebar2 "${rebar_version}" "${otp_version}"
fi

if [ "${rebar_name}" = "rebar3" ]; then
  build_rebar3 "${rebar_version}" "${otp_version}"
fi

rebar_csv "${rebar_name}" "${rebar_version}" "${elixir_version}"
upload    "${rebar_name}" "${rebar_version}" "${elixir_version}"
