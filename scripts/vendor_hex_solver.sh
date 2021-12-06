#!/bin/bash
set -e

if [[ -z "$1" ]]; then
  echo "Usage: vendor_hex_solver.sh PATH_TO_HEX_SOLVER"
  exit 1
fi

dir=$1

pushd $dir
mix compile
version=$(mix run -e 'IO.puts(Mix.Project.config[:version])')
shortref=$(git rev-parse --short HEAD)
popd

rm -f lib/hex/solver.ex
rm -rf lib/hex/solver

skip_filenames="hex_solver/dev.ex"

for filename in $(find $dir/lib -type f); do
  target_filename=${filename#$dir/lib/}
  target_path=lib/hex/${target_filename/hex_solver/solver}

  if [[ $skip_filenames == *$target_filename* ]]; then
    continue
  fi

  mkdir -p $(dirname $target_path)
  echo "# Vendored from hex_solver v$version ($shortref), do not edit manually" > $target_path
  echo >> $target_path
  cat $filename >> $target_path

  sed -i.bak 's/@moduledoc """/_ = """/g' $target_path
  rm $target_path.bak

  sed -i.bak s/HexSolver/Hex.Solver/g $target_path
  rm $target_path.bak
done
