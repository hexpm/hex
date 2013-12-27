defmodule ExplexTest do
  use ExUnit.Case, async: false

  _packages = [
    { :postgrex, "0.0.1", [] },
    { :postgrex, "0.1.0", [] },
    { :postgrex, "0.1.1", [] },
    { :postgrex, "0.1.2", [] },
    { :postgrex, "0.2.0", [] },
    { :ecto, "0.0.1", [] },
    { :ecto, "0.0.2", [] },
    { :ecto, "0.1.0", [{ :postgrex, "~> 0.1.0"}] },
    { :ecto, "0.1.2", [{ :postgrex, "~> 0.1.0"}] },
    { :ecto, "0.2.0", [{ :postgrex, "~> 0.3.0"}] },
  ]

  defp insert_data(packages) do
    Enum.map(packages, &Tuple.insert_at(&1, 0, Explex.Package))
      |> Explex.add_packages
  end

  setup do
    Explex.start
    :ok
  end

  teardown do
    Explex.stop
    :ok
  end

  test "simple" do
    insert_data [
      { :postgrex, "0.0.1", [] },
      { :postgrex, "0.1.0", [] },
      { :postgrex, "0.2.0", [] },
      { :postgrex, "0.2.1", [] },
      { :ecto, "0.0.1", [] },
      { :ecto, "0.1.0", [{ :postgrex, "~> 0.1.0"}] },
      { :ecto, "0.2.0", [{ :postgrex, "~> 0.2.0"}] },
    ]

    deps = [postgrex: nil, ecto: nil]
    assert [postgrex: "0.2.1", ecto: "0.2.0"] = Explex.resolve(deps)

    deps = [postgrex: "0.2.1", ecto: "0.2.0"]
    assert [postgrex: "0.2.1", ecto: "0.2.0"] = Explex.resolve(deps)

    deps = [postgrex: "0.2.0", ecto: "0.2.0"]
    assert [postgrex: "0.2.0", ecto: "0.2.0"] = Explex.resolve(deps)

    deps = [postgrex: "~> 0.3.0", ecto: nil]
    assert nil = Explex.resolve(deps)

    deps = [postgrex: nil, ecto: "~> 0.3.0"]
    assert nil = Explex.resolve(deps)
  end

  test "backtrack" do
    insert_data [
      { :postgrex, "0.0.1", [] },
      { :postgrex, "0.1.0", [] },
      { :postgrex, "0.2.0", [] },
      { :postgrex, "0.2.1", [] },
      { :ecto, "0.0.1", [] },
      { :ecto, "0.0.2", [{ :postgrex, "0.1.1"}] },
      { :ecto, "0.1.0", [{ :postgrex, "~> 0.1.0"}] },
      { :ecto, "0.1.2", [{ :postgrex, "~> 0.1.0"}] },
      { :ecto, "0.2.0", [{ :postgrex, "~> 0.2.0"}] },
    ]

    deps = [postgrex: "0.2.0", ecto: "0.2.0"]
    assert [postgrex: "0.2.0", ecto: "0.2.0"] = Explex.resolve(deps)

    deps = [postgrex: "0.1.0", ecto: ">= 0.1.0"]
    assert [postgrex: "0.1.0", ecto: "0.1.2"] = Explex.resolve(deps)

    deps = [postgrex: nil, ecto: "< 0.1.0"]
    assert [postgrex: "0.2.1", ecto: "0.0.1"] = Explex.resolve(deps)

    deps = [postgrex: "0.1.0", ecto: "< 0.1.0"]
    assert [postgrex: "0.1.0", ecto: "0.0.1"] = Explex.resolve(deps)

    deps = [postgrex: "0.1.0", ecto: "~> 0.0.2"]
    assert nil = Explex.resolve(deps)

    deps = [postgrex: nil, ecto: "0.0.2"]
    assert nil = Explex.resolve(deps)
  end

  test "complete backtrack" do
    insert_data [
      { :postgrex, "0.2.0", [] },
      { :postgrex, "0.2.1", [] },
      { :ecto, "0.0.1", [] },
      { :ecto, "0.0.2", [] },
      { :ecto, "0.1.0", [{ :postgrex, "~> 0.1.0"}] },
      { :ecto, "0.1.2", [{ :postgrex, "~> 0.1.0"}] },
      { :ecto, "0.2.0", [{ :postgrex, "~> 0.3.0"}] },
    ]

    deps = [postgrex: nil, ecto: nil]
    assert [postgrex: "0.2.1", ecto: "0.0.2"] = Explex.resolve(deps)
  end
end
