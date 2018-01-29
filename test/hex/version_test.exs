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
end
