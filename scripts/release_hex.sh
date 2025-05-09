#!/usr/bin/env bash

# Usage:
#
#     $ ELIXIR_PEM=/path/to/elixir.pem \
#       HEX_FASTLY_KEY=... \
#       HEX_FASTLY_BUILDS_SERVICE_ID=... \
#         release_hex.sh HEX_VERSION
#
# Unless ELIXIR_PEM is set, nothing is uploaded. After running, can be locally tested:
#
#     $ (cd tmp && erl -S httpd)
#     $ HEX_BUILDS_URL=http://localhost:8000 mix local.hex --force

set -e -u -o pipefail

function main {
  hex_version=$1
  installs_dir="$PWD/tmp/installs"
  hex_csv="${installs_dir}/hex.csv"

  rm -rf "${installs_dir}"
  mkdir "${installs_dir}"

  s3down hex.csv "${hex_csv}"
  touch "${hex_csv}"
  sed -i.bak "/^${hex_version},/d" "${hex_csv}"

  # UPDATE THIS FOR EVERY RELEASE, ORDER MATTERS

  # Elixir v1.17
  build ${hex_version} 25.3.2.20 1.17.3 1.17.0 noble-20250404
  build ${hex_version} 26.2.5.11 1.17.3 1.17.0 noble-20250404
  build ${hex_version} 27.3.3    1.17.3 1.17.0 noble-20250404

  # Elixir v1.18
  build ${hex_version} 25.3.2.16 1.18.0 1.18.0 noble-20241015 # need to use exactly 1.18.0 and that requires older otp & ubuntu
  build ${hex_version} 26.2.5.6  1.18.0 1.18.0 noble-20241015 # ditto
  build ${hex_version} 27.2      1.18.0 1.18.0 noble-20241015 # ditto

  rm -rf _build
  rm "${hex_csv}.bak"

  if [ -n "${ELIXIR_PEM}" ]; then
    openssl dgst -sha512 -sign "${ELIXIR_PEM}" "${hex_csv}" | openssl base64 > "${hex_csv}.signed"

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

# $1 = hex version
# $2 = erlang version
# $3 = elixir version
# $4 = saved elixir version
# $5 = ubuntu version
function build {
  hex_version=$1
  otp_version=$2
  otp_release=${otp_version%%.*}
  elixir_version=$3
  saved_elixir_version=$4
  ubuntu_version=$5

  echo "Building ${elixir_version} ${otp_version} ${ubuntu_version}"
  rm -rf _build src/mix_safe_erl_term.erl
  hex_ez=hex-${hex_version}-otp-${otp_release}.ez

  mkdir -p "$installs_dir/${saved_elixir_version}"
  docker run -v $(pwd):/hex hexpm/elixir:${elixir_version}-erlang-${otp_version}-ubuntu-${ubuntu_version} sh -c " \
    cd /hex && \
    MIX_ENV=prod mix archive.build -o ${hex_ez}"

  mv "${hex_ez}" "${installs_dir}/${saved_elixir_version}/${hex_ez}"
  sha=$(shasum -a 512 "${installs_dir}/${saved_elixir_version}/${hex_ez}")
  sha=($sha)
  echo "${hex_version},${sha},${saved_elixir_version},${otp_release}" >> "${hex_csv}"
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
