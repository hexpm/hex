defmodule Mix.Tasks.Hex.BuildTest do
  use HexTest.Case
  @moduletag :integration

  defp package_created?(name) do
    File.exists?("#{name}.tar")
  end

  test "create" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      Mix.Tasks.Hex.Build.run([])
      assert package_created?("release_a-0.0.1")
    end
  after
    purge [ReleaseSimple.Mixfile]
  end

  test "create with package name" do
    Mix.Project.push ReleaseName.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      Mix.Tasks.Hex.Build.run([])
      assert package_created?("released_name-0.0.1")
    end
  after
    purge [ReleaseName.Mixfile]
  end

  test "create with deps" do
    Mix.Project.push ReleaseDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      Mix.Tasks.Deps.Get.run([])

      Mix.Tasks.Hex.Build.run([])

      assert_received {:mix_shell, :info, ["\e[33m  WARNING! No files\e[0m"]}
      assert_received {:mix_shell, :info, ["\e[33m  WARNING! Missing metadata fields: maintainers, links\e[0m"]}
      assert package_created?("release_b-0.0.2")
    end
  after
    purge [ReleaseDeps.Mixfile]
  end

  test "create with meta" do
    Mix.Project.push ReleaseMeta.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      File.write!("myfile.txt", "hello")
      Mix.Tasks.Hex.Build.run([])

      assert_received {:mix_shell, :info, ["Building release_c 0.0.3"]}
      assert_received {:mix_shell, :info, ["  Files:"]}
      assert_received {:mix_shell, :info, ["    myfile.txt"]}
      assert_received {:mix_shell, :info, ["\e[33m  WARNING! Missing files: missing.txt, missing/*" <> _]}
      refute_received {:mix_shell, :info, ["\e[33m  WARNING! Missing metadata fields" <> _]}
      assert package_created?("release_c-0.0.3")
    end
  after
    purge [ReleaseMeta.Mixfile]
  end

  test "reject package if description is missing" do
    Mix.Project.push ReleaseNoDescription.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      assert_raise Mix.Error, "Stopping package build due to errors", fn ->
        Mix.Tasks.Hex.Build.run([])

        assert_received {:mix_shell, :info, ["Building release_e 0.0.1"]}
        assert_received {:mix_shell, :error, ["  ERROR! Missing metadata fields: description"]}

        refute package_created?("release_e-0.0.1")
      end
    end
  after
    purge [ReleaseNoDescription.Mixfile]
  end

  test "warn if description is too long" do
    Mix.Project.push ReleaseTooLongDescription.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      Mix.Tasks.Hex.Build.run([])

      assert_received {:mix_shell, :info, ["\e[33m  WARNING! Package description is very long (exceeds " <> _]}
      assert package_created?("release_f-0.0.1")
    end
  after
    purge [ReleaseTooLongDescription.Mixfile]
  end
end
