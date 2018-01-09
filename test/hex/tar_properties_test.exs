{:ok, system_version} = Version.parse(System.version)
elixir_version = {system_version.major, system_version.minor, system_version.patch}

if elixir_version >= {1, 5, 0} do
  {:ok, _} = Application.ensure_all_started(:stream_data)

  defmodule Hex.TarPropertiesTest do
    use HexTest.Case, async: true
    use ExUnitProperties

    property "gzip is circular" do
      check all binary <- binary() do
        assert :zlib.gunzip(Hex.Tar.gzip(binary)) == binary
      end
    end

    property "Hex.Tar is equivalent to zlib" do
      check all binary <- binary() do
        assert <<31, 139, 8, 0, 0, 0, 0, 0, 0, _os, zlib_rest::binary>> = :zlib.gzip(binary)
        assert <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0, hex_rest::binary>> = Hex.Tar.gzip(binary)
        assert zlib_rest == hex_rest
      end
    end
  end
end
