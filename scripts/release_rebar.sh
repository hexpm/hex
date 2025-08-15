#!/usr/bin/env bash
# TODO: rename to scripts/release_rebar.sh. publish_rebar.sh is a temporary name for prettier git diff.

# Usage:
#
#     $ ELIXIR_PEM=/path/to/elixir.pem \
#        HEX_FASTLY_KEY=... \
#        HEX_FASTLY_BUILDS_SERVICE_ID=... \
#        release_rebar.sh
#
# Unless ELIXIR_PEM is set, nothing is uploaded. After running, can be locally tested:
#
#     $ (cd tmp && erl -S httpd)
#     $ HEX_BUILDS_URL=http://localhost:8000 mix local.rebar --force

set -e -u -o pipefail

function main {
  installs_dir="$PWD/tmp/installs"
  rebar_csv="${installs_dir}/rebar.csv"

  rm -rf "${installs_dir}"
  mkdir "${installs_dir}"

  # rebar.csv is always rebuilt from scratch. rebar builds are not reproducible, unfortunately.
  touch "${rebar_csv}"

  # UPDATE THIS FOR EVERY RELEASE
  build 3.22.0 25.3.2.20 1.18.3 noble-20250404
  build 3.22.0 26.2.5.11 1.18.3 noble-20250404

  build 3.24.0 25.3.2.20 1.18.3 noble-20250404
  build 3.24.0 26.2.5.11 1.18.3 noble-20250404
  build 3.24.0 27.3.3    1.18.3 noble-20250404
  build 3.25.1 28.0.1    1.18.4 noble-20250714

  if [ -n "${ELIXIR_PEM}" ]; then
    openssl dgst -sha512 -sign "${ELIXIR_PEM}" "${rebar_csv}" | openssl base64 > "${rebar_csv}.signed"
    cd $installs_dir
    for path in $(find . -type f | sort); do
      path="${path#./}"
      echo "uploading ${path}..."
      s3up "${path}" "${path}"
    done

    purge_key "${HEX_FASTLY_BUILDS_SERVICE_ID}" "installs"
  else
    echo "ELIXIR_PEM is empty, skipping"
    exit 1
  fi
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
