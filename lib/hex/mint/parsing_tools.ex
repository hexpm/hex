# Vendored from mint v1.11.0 (fb850d3), do not edit manually
# Copyright 2018 Eric Meadows-Jönsson and Andrea Leopardi
# Licensed under the Apache License, Version 2.0, modified by scripts/vendor_mint.sh

defmodule Hex.Mint.ParsingTools do
  @moduledoc false

  @spec only_digits?(binary()) :: boolean()
  def only_digits?(<<char>>) when char in ?0..?9, do: true
  def only_digits?(<<char, rest::binary>>) when char in ?0..?9, do: only_digits?(rest)
  def only_digits?(_other), do: false
end
