defmodule Mix.Tasks.Hex.BuildTest do
  use HexTest.Case
  @moduletag :integration

  defmodule ReleaseSimple.Mixfile do
    def project do
      [ app: :releasea, version: "0.0.1" ]
    end
  end

  defmodule ReleaseDeps.Mixfile do
    def project do
      [ app: :releaseb, version: "0.0.2",
        deps: [{:ex_doc, "0.0.1", package: true}] ]
    end
  end

  defmodule ReleaseMeta.Mixfile do
    def project do
      [ app: :releasec, version: "0.0.3",
        description: "foo",
        package: [files: ["myfile.txt", "missing.txt", "missing/*"],
                  licenses: ["Apache"],
                  links: %{"a" => "b"},
                  maintainers: ["maintainers"]] ]
    end
  end

  defmodule ReleaseName.Mixfile do
    def project do
      [ app: :released, version: "0.0.1",
        package: [name: :released_name] ]
    end
  end

  defp package_created?(name) do
    File.exists?("#{name}.tar")
  end

  setup do
    Hex.Registry.open!(registry_path: tmp_path("registry.ets"))
    :ok
  end

  test "create" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      Mix.Tasks.Hex.Build.run([])
      assert package_created?("releasea-0.0.1")
    end
  end

  test "create with package name" do
    Mix.Project.push ReleaseName.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      Mix.Tasks.Hex.Build.run([])
      assert package_created?("released_name-0.0.1")
    end
  end

  test "create with deps" do
    Mix.Project.push ReleaseDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      Mix.Tasks.Deps.Get.run([])

      Mix.Tasks.Hex.Build.run([])

      assert_received {:mix_shell, :info, ["\e[33m  WARNING! No files\e[0m"]}
      assert_received {:mix_shell, :info, ["\e[33m  WARNING! Missing metadata fields: description, licenses, maintainers, links\e[0m"]}
      assert package_created?("releaseb-0.0.2")
    end
  after
    purge [Ex_doc.NoConflict.Mixfile]
  end

  test "create with meta" do
    Mix.Project.push ReleaseMeta.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      File.write!("myfile.txt", "hello")
      Mix.Tasks.Hex.Build.run([])

      assert_received {:mix_shell, :info, ["Building releasec v0.0.3"]}
      assert_received {:mix_shell, :info, ["  Files:"]}
      assert_received {:mix_shell, :info, ["    myfile.txt"]}
      assert_received {:mix_shell, :info, ["\e[33m  WARNING! Missing files: missing.txt, missing/*" <> _]}
      refute_received {:mix_shell, :info, ["\e[33m  WARNING! Missing metadata fields" <> _]}
      assert package_created?("releasec-0.0.3")
    end
  end
end
