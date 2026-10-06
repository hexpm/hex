defmodule Hex.ModuledocTest do
  use ExUnit.Case, async: true

  # Hex's documentation is built by running ExDoc on the compiled modules
  # (see scripts/release_docs.sh). Only the Mix tasks are public, every other
  # module under lib/ must be @moduledoc false or ExDoc lists it. ExDoc skips
  # protocol implementations regardless of their moduledoc.
  test "only Mix tasks under lib/ are documented" do
    {:ok, modules} = :application.get_key(:hex, :modules)
    lib = Path.expand("lib") <> "/"

    documented =
      Enum.filter(modules, fn module ->
        source = List.to_string(module.module_info(:compile)[:source])

        String.starts_with?(source, lib) and
          not function_exported?(module, :__impl__, 1) and
          not match?("Elixir.Mix.Tasks." <> _, Atom.to_string(module)) and
          match?({:docs_v1, _, _, _, doc, _, _} when doc != :hidden, Code.fetch_docs(module))
      end)

    assert documented == []
  end
end
