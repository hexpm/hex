#!/usr/bin/env bash

# Usage:
#
#     $ ELIXIR_PEM=/path/to/elixir.pem \
#        HEX_FASTLY_KEY=... \
#        HEX_FASTLY_BUILDS_SERVICE_ID=... \
#        release_rebar.sh
# Unless HEX_FASTLY_KEY is set, nothing is uploaded. After running, can be locally tested:
#
#     $ (cd tmp && erl -S httpd)
#     $ HEX_BUILDS_URL=http://localhost:8000 mix local.rebar --force

set -e -u -o pipefail

function main {
  installs_dir="$PWD/tmp/installs"
  rebar_csv="${installs_dir}/rebar.csv"
  rebar_1x_csv="${installs_dir}/rebar3-1.x.csv"

  rm -rf "${installs_dir}"
  mkdir "${installs_dir}"

  touch "${rebar_csv}"

  s3down rebar3-1.x.csv "${rebar_1x_csv}" || true
  touch "${rebar_1x_csv}"

  # UPDATE THIS FOR EVERY RELEASE

  # Old Elixir versions (old CSV format)
  build_old 3.13.3  17.5.6.10  1.0.0        xenial-20200326
  build_old 3.15.2  21.3.8.21  1.11.4       xenial-20201014
  build_old 3.15.2  22.3.4.22  1.13.0       xenial-20210114
  build_old 3.22.0  23.3.4.18  1.14.5       xenial-20210804
  build_old 3.22.0  24.3.4.11  1.15.0-rc.0  xenial-20210804

  # New Elixir versions (new CSV format)
  build 3.22.0 25.3.2.20 1.18.3 noble-20250404
  build 3.22.0 26.2.5.11 1.18.3 noble-20250404
  build 3.24.0 25.3.2.20 1.18.3 noble-20250404
  build 3.24.0 26.2.5.11 1.18.3 noble-20250404
  build 3.24.0 27.3.3    1.18.3 noble-20250404
  build 3.25.1 28.0.1    1.18.4 noble-20250714

  rm -f "${rebar_1x_csv}.bak"

  openssl dgst -sha512 -sign "${ELIXIR_PEM}" "${rebar_csv}" | openssl base64 > "${rebar_csv}.signed"
  openssl dgst -sha512 -sign "${ELIXIR_PEM}" "${rebar_1x_csv}" | openssl base64 > "${rebar_1x_csv}.signed"
  cd $installs_dir
  for path in $(find . -type f | sort); do
    path="${path#./}"

    if [ -n "${HEX_FASTLY_KEY-}" ]; then
      echo "uploading ${path}..."
      s3up "${path}" "${path}"
    else
      echo "[skip] uploading ${path}..."
    fi
  done

  if [ -n "${HEX_FASTLY_KEY-}" ]; then
    purge_key "${HEX_FASTLY_BUILDS_SERVICE_ID}" "installs"
  fi
}

# $1 = rebar version
# $2 = erlang version
# $3 = elixir version
# $4 = ubuntu version
function build_old {
  rebar_version=$1
  otp_version=$2
  elixir_version=$3
  ubuntu_version=$4

  echo "Building ${rebar_version} ${otp_version} ${ubuntu_version} (old format)"
  mkdir -p "${installs_dir}/${elixir_version}"

  docker run -v "${installs_dir}/${elixir_version}":/installs hexpm/erlang:${otp_version}-ubuntu-${ubuntu_version} sh -c "\
    apt update && apt -y install git && \
    git clone https://github.com/erlang/rebar3.git -b ${rebar_version} && \
    cd rebar3 && \
    ./bootstrap && \
    cp rebar3 /installs/rebar3 && \
    cp rebar3 /installs/rebar3-${rebar_version}
    "

  sed -i.bak "/,${elixir_version}\$/d" "${rebar_1x_csv}"
  sha=$(shasum -a 512 "${installs_dir}/${elixir_version}/rebar3")
  sha=($sha)
  echo "${rebar_version},${sha},${elixir_version}" >> "${rebar_1x_csv}"
}

# $1 = rebar version
# $2 = erlang version
# $3 = elixir version
# $4 = ubuntu version
function build {
  rebar_version=$1
  otp_version=$2
  otp_release=${otp_version%%.*}
  elixir_version=$3
  ubuntu_version=$4

  echo "Building ${rebar_version} ${otp_version} ${ubuntu_version}"
  mkdir -p "${installs_dir}/${elixir_version}"
  rebar="rebar3-${rebar_version}-otp-${otp_release}"

  docker run -v "${installs_dir}/${elixir_version}":/installs hexpm/erlang:${otp_version}-ubuntu-${ubuntu_version} sh -c "\
    apt update && apt -y install git && \
    git clone https://github.com/erlang/rebar3.git -b ${rebar_version} && \
    cd rebar3 && \
    ./bootstrap && \
    cp rebar3 /installs/$rebar
    "

  sha=$(shasum -a 512 "${installs_dir}/${elixir_version}/$rebar")
  sha=($sha)
  echo "${rebar_version},${sha},${elixir_version},${otp_release}" >> "${rebar_csv}"
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

# $1 = service
# $2 = key
function purge_key() {
  curl \
    --fail \
    -X POST \
    -H "Fastly-Key: ${HEX_FASTLY_KEY}" \
    -H "Accept: application/json" \
    -H "Content-Length: 0" \
    "https://api.fastly.com/service/$1/purge/$2"
}

main $*
