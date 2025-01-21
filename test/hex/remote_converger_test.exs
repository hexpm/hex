defmodule Hex.RemoteConvergetTest do
  use HexTest.IntegrationCase

  defmodule OutdatedDepsBefore.MixProject do
    def project do
      [
        app: :outdated_deps,
        version: "0.1.0",
        deps: [
          {:postgrex, "0.2.1", warn_if_outdated: true},
          {:ecto, "3.3.1", warn_if_outdated: true},
          {:ecto_sql, "3.3.2", warn_if_outdated: true}
        ]
      ]
    end
  end

  defmodule OutdatedDepsAfter.MixProject do
    def project do
      [
        app: :outdated_deps,
        version: "0.1.0",
        deps: [
          {:postgrex, ">= 0.0.0", warn_if_outdated: true},
          {:ecto, ">= 0.0.0", warn_if_outdated: true},
          {:ecto_sql, ">= 0.0.0", warn_if_outdated: true}
        ]
      ]
    end
  end

  test "deps with warn_if_outdated: true" do
    in_tmp(fn ->
      Mix.Project.push(OutdatedDepsBefore.MixProject)
      :ok = Mix.Tasks.Deps.Get.run([])

      Mix.Project.pop()
      Mix.Project.push(OutdatedDepsAfter.MixProject)

      output =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          :ok = Mix.Tasks.Deps.Get.run([])
        end)

      assert output =~ "ecto 3.3.2 is available"
      assert output =~ "ecto_sql 3.3.3 is available"
      refute output =~ "postgrex"
    end)
  end
end
