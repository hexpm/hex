#!/usr/bin/env bash

set -e -u

# $1 = rebar name
# $2 = rebar version
# $3 = elixir version
function rebar_csv {
  if [ -f "rebar3-1.x.csv" ]; then
    mv rebar3-1.x.csv rebar3-1.x-old.csv
  else
    s3down "${1}-1.x.csv" "${1}-1.x-old.csv"
  fi

  # Remove existing build for this elixir version
  cat ${1}-1.x-old.csv | grep -v ",${3}\$" > ${1}-1.x.csv
  rm ${1}-1.x-old.csv

  # Add new build
  sha=$(shasum -a 512 "${1}")
  sha=($sha)
  echo "${2},${sha},${3}" >> ${1}-1.x.csv
}

# $1 = rebar name
function sign_csv {
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
# $3 = ubuntu version
function build_rebar2 {
  docker rm rebar2 || true

  docker run --name rebar2 hexpm/erlang:$2-ubuntu-$3 sh -c "\
    apt update && apt -y install git && \
    git clone https://github.com/rebar/rebar.git -b $1 && \
    cd rebar && \
    ./bootstrap"

  docker cp rebar2:/rebar/rebar rebar
  docker rm rebar2
}

# $1 = rebar version
# $2 = otp version
# $3 = ubuntu version
function build_rebar3 {
  docker rm rebar3 || true

  docker run --name rebar3 hexpm/erlang:$2-ubuntu-$3 sh -c "\
    apt update && apt -y install git && \
    git clone https://github.com/erlang/rebar3.git -b $1 && \
    cd rebar3 && \
    ./bootstrap"

  docker cp rebar3:/rebar3/rebar3 rebar3
  docker rm rebar3
}

rm rebar rebar3 rebar-1.x.csv rebar-1.x.csv.signed

# Update these values for every release
# Call as ELIXIR_PEM=elixir.pem ./release_rebar.sh

# Note that the order of versions in the CSV file matters so all versions
# of rebar needs to be updated.

# For Elixir 1.0.0 / rebar 3.13.3
rebar_name="rebar3"
rebar_version="3.13.3"
elixir_version="1.0.0"
otp_version="17.5.6.10"
ubuntu_version="xenial-20200326"
build_rebar3 "${rebar_version}" "${otp_version}" "${ubuntu_version}"
rebar_csv rebar3 "${rebar_version}" "${elixir_version}"

# For Elixir 1.11.4 / rebar 3.15.2
rebar_name="rebar3"
rebar_version="3.15.2"
elixir_version="1.11.4"
otp_version="21.3.8.21"
ubuntu_version="xenial-20201014"
build_rebar3 "${rebar_version}" "${otp_version}" "${ubuntu_version}"
rebar_csv rebar3 "${rebar_version}" "${elixir_version}"

# For Elixir 1.13.0 / rebar 3.15.2
rebar_name="rebar3"
rebar_version="3.15.2"
elixir_version="1.13.0"
otp_version="22.3.4.22"
ubuntu_version="xenial-20210114"
build_rebar3 "${rebar_version}" "${otp_version}" "${ubuntu_version}"
rebar_csv rebar3 "${rebar_version}" "${elixir_version}"

sign_csv rebar3
upload rebar3 "${rebar_version}" "${elixir_version}"

# build_rebar2 "${rebar_version}" "${otp_version}" "${ubuntu_version}"
# rebar_csv rebar2 "${rebar_version}" "${elixir_version}"
# upload rebar2 "${rebar_version}" "${elixir_version}"
