#!/bin/bash
set -e

# Pending upstream Mint PRs that must be present in the source tree passed
# to this script. If re-vendoring from upstream `main` or a Mint release that
# does not yet include these, either wait for them to merge or point this
# script at a branch that has them applied:
#
#   * elixir-mint/mint#478 - "Support Elixir ~> 1.12". Removes the
#     `^length` binary-size pin (Elixir 1.14+) and drops `Mint.Application`
#     so the persistent_term cacertfile cache works without the app starting.
#     Hex supports `~> 1.12` so these changes are required.
#     https://github.com/elixir-mint/mint/pull/478
#     Fork branch: https://github.com/elixir-mint/mint/tree/ericmj/support-elixir-1.12
#
#   * elixir-mint/mint#479 - "Fix HTTP/1 handling of 1xx informational
#     responses". Without this, a 100 Continue / 103 Early Hints / any
#     unsolicited 1xx causes Mint to emit `:done` prematurely and close
#     the connection on the real final response. See also the HEX PATCH
#     comments in lib/hex/mint/http1.ex.
#     https://github.com/elixir-mint/mint/pull/479
#     Fork branch: https://github.com/elixir-mint/mint/tree/ericmj/fix-1xx-informational-response
#
#   * `:connection_window_size` option on HTTP/2 connect - local patch,
#     not yet upstreamed. Allows the caller to bump the HTTP/2
#     connection-level receive window beyond the spec-mandated 64 KB
#     default. Without this, any multi-MB download stalls every ~64 KB
#     waiting for an auto WINDOW_UPDATE round-trip, making HTTP/2
#     throughput much worse than HTTP/1 for hex's tarball workload.
#     See HEX PATCH in lib/hex/mint/http2.ex `initiate/5`.
#     TODO: upstream as a Mint PR.

if [[ -z "$1" ]]; then
  echo "Usage: vendor_mint.sh PATH_TO_MINT"
  exit 1
fi

dir=$1

pushd $dir
mix compile
version=$(mix run -e 'IO.puts(Mix.Project.config[:version])')
shortref=$(git rev-parse --short HEAD)
popd

rm -rf lib/hex/mint

skip_filenames="mint/application.ex"

for filename in $(find $dir/lib -type f -name '*.ex'); do
  target_filename=${filename#$dir/lib/}
  target_path=lib/hex/${target_filename}

  if [[ $skip_filenames == *$target_filename* ]]; then
    continue
  fi

  mkdir -p $(dirname $target_path)
  echo "# Vendored from mint v$version ($shortref), do not edit manually" > $target_path
  echo >> $target_path
  cat $filename >> $target_path

  # Suppress @moduledoc
  sed -i.bak 's/@moduledoc """/_ = """/g' $target_path
  rm $target_path.bak

  # Rename modules: Mint.* -> Hex.Mint.*
  sed -i.bak 's/Mint\./Hex.Mint./g' $target_path
  rm $target_path.bak

  # Rename internal atom tags used in throw/catch and persistent_term keys
  sed -i.bak 's/{:mint,/{:hex_mint,/g' $target_path
  rm $target_path.bak

  # Use vendored Mint version in user agent
  sed -i.bak "s|Mix\.Project\.config()\[:version\]|\"$version\"|g" $target_path
  rm $target_path.bak
done

# Vendor Erlang shims
for filename in $(find $dir/src -type f -name '*.erl'); do
  basename=$(basename $filename)
  target_path=src/hex_${basename}

  echo "%% Vendored from mint v$version ($shortref), do not edit manually" > $target_path
  echo >> $target_path
  cat $filename >> $target_path

  sed -i.bak 's/-module(mint_/-module(hex_mint_/g' $target_path
  rm $target_path.bak
done

# Update references to Erlang shims in vendored Elixir files
for filename in $(find lib/hex/mint -type f -name '*.ex'); do
  sed -i.bak 's/:mint_shims/:hex_mint_shims/g' $filename
  rm $filename.bak
done

# Vendor hpax (Mint dependency for HTTP/2 HPACK)
hpax_dir=$dir/deps/hpax
hpax_version=$(cat $hpax_dir/mix.exs | grep '@version' | head -1 | sed 's/.*"\(.*\)".*/\1/')

for filename in $(find $hpax_dir/lib -type f -name '*.ex'); do
  target_filename=${filename#$hpax_dir/lib/}
  target_path=lib/hex/mint/${target_filename}

  mkdir -p $(dirname $target_path)
  echo "# Vendored from hpax v$hpax_version, do not edit manually" > $target_path
  echo >> $target_path
  cat $filename >> $target_path

  # Suppress @moduledoc
  sed -i.bak 's/@moduledoc """/_ = """/g' $target_path
  rm $target_path.bak

  # Rename modules: HPAX -> Hex.Mint.HPAX
  sed -i.bak 's/HPAX/Hex.Mint.HPAX/g' $target_path
  rm $target_path.bak
done

# Copy non-source files needed at compile time
cp $hpax_dir/lib/hpax/huffman_table lib/hex/mint/hpax/huffman_table

# Also rename HPAX references in vendored Mint files
for filename in $(find lib/hex/mint -type f -name '*.ex' ! -path '*/hpax*'); do
  sed -i.bak 's/HPAX/Hex.Mint.HPAX/g' $filename
  rm $filename.bak
done
