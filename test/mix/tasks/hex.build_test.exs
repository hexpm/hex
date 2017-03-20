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

      error_msg = "Stopping package build due to errors.\n" <>
                  "Missing metadata fields: maintainers, links"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])

        assert_received {:mix_shell, :error, ["No files"]}
        refute package_created?("release_b-0.0.2")
      end
    end
  after
    purge [ReleaseDeps.Mixfile]
  end

  test "create with meta" do
    Mix.Project.push ReleaseMeta.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      error_msg = "Stopping package build due to errors.\n" <>
                  "Missing files: missing.txt, missing/*"

      assert_raise Mix.Error, error_msg, fn ->
        File.write!("myfile.txt", "hello")
        Mix.Tasks.Hex.Build.run([])

        assert_received {:mix_shell, :info, ["Building release_c 0.0.3"]}
        assert_received {:mix_shell, :info, ["  Files:"]}
        assert_received {:mix_shell, :info, ["    myfile.txt"]}
      end
    end
  after
    purge [ReleaseMeta.Mixfile]
  end

  test "reject package if description is missing" do
    Mix.Project.push ReleaseNoDescription.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      error_msg = "Stopping package build due to errors.\n" <>
                  "Missing metadata fields: description, licenses, maintainers, links"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])

        assert_received {:mix_shell, :info, ["Building release_e 0.0.1"]}

        refute package_created?("release_e-0.0.1")
      end
    end
  after
    purge [ReleaseNoDescription.Mixfile]
  end

  test "error if description is too long" do
    Mix.Project.push ReleaseTooLongDescription.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      error_msg = "Stopping package build due to errors.\n" <>
                  "Missing metadata fields: licenses, maintainers, links\n" <>
                  "Package description is very long (exceeds 300 characters)"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end
  after
    purge [ReleaseTooLongDescription.Mixfile]
  end

  test "error if package has unstable dependencies" do
    Mix.Project.push ReleasePreDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      error_msg = "A stable package release cannot have a pre-release dependency."

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end
  after
    purge [ReleasePreDeps.Mixfile]
  end
end
