defmodule Hex.VersionTest do
  use ExUnit.Case, async: true

  alias Hex.Version, as: V

  test "pre releases" do
    assert V.match?("1.0.1", "~> 1.0.0")
    refute V.match?("1.0.1-beta", "~> 1.0.0")
    assert V.match?("1.0.1-beta", "~> 1.0.0-beta")

    assert V.match?("1.1.0", ">= 1.0.0")
    refute V.match?("1.1.0-beta", ">= 1.0.0")
    assert V.match?("1.1.0-beta", ">= 1.0.0-beta")
  end

  test "parse requirement" do
    assert {:ok, _} = V.parse_requirement("1.0.0")
    assert {:ok, _} = V.parse_requirement("== 1.0.0")
    assert {:ok, _} = V.parse_requirement("==1.0.0")
    assert {:ok, _} = V.parse_requirement("== 1.0.0 and == 1.0.0")
    assert {:ok, _} = V.parse_requirement("==1.0.0 and ==1.0.0")

    assert :error = V.parse_requirement("foo")
  end

  test "major_version_change?/2" do
    {:ok, ver1} = Version.parse("0.1.0")
    {:ok, ver2} = Version.parse("0.2.0")
    {:ok, ver3} = Version.parse("1.0.0")
    {:ok, ver4} = Version.parse("1.1.0")
    {:ok, ver5} = Version.parse("2.2.0")

    assert V.major_version_change?(ver3, ver5)
    assert V.major_version_change?(ver1, ver3)
    refute V.major_version_change?(ver1, ver2)
    refute V.major_version_change?(ver3, ver4)
  end

  test "breaking_minor_version_change?/2" do
    {:ok, ver1} = Version.parse("0.1.0")
    {:ok, ver2} = Version.parse("0.2.0")
    {:ok, ver3} = Version.parse("1.0.0")
    {:ok, ver4} = Version.parse("1.1.0")
    {:ok, ver5} = Version.parse("2.2.0")

    assert V.breaking_minor_version_change?(ver1, ver2)
    assert V.breaking_minor_version_change?(ver2, ver1)
    refute V.breaking_minor_version_change?(ver3, ver4)
    refute V.breaking_minor_version_change?(ver3, ver5)
  end
end
