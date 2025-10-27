defmodule Hex.Registry.ErrorMessageTest do
  use HexTest.Case

  setup do
    Application.ensure_all_started(:hex)

    Hex.State.put(:repos, %{
      "hexpm" => %{
        url: "https://repo.hex.pm",
        public_key: nil,
        auth_key: nil
      },
      "hexpm:acme" => %{
        url: "https://repo.hex.pm/repos/acme",
        public_key: nil,
        auth_key: nil
      }
    })

    :ok
  end

  describe "404 errors (package not found)" do
    test "displays helpful message for non-existent package" do
      result = {:ok, {404, [], %{}}}
      send_error_message(result, "hexpm", "nonexistent_package", false)

      assert_received {:mix_shell, :error, [msg]}
      assert msg =~ "Failed to fetch record for nonexistent_package from registry"

      assert_received {:mix_shell, :error, [msg]}
      assert msg =~ "The package nonexistent_package does not exist"
      assert msg =~ "Please verify the package name is spelled correctly"
    end

    test "displays helpful message for non-existent package with cache" do
      package_name = "typo_package"

      result = {:ok, {404, [], %{}}}
      send_error_message(result, "hexpm", package_name, true)

      assert_received {:mix_shell, :error, [msg]}

      assert msg =~
               "Failed to fetch record for #{package_name} from registry (using cache instead)"

      # Still shows detailed diagnostics to help user understand the issue
      assert_received {:mix_shell, :error, [msg]}
      assert msg =~ "The package #{package_name} does not exist"
      assert msg =~ "Please verify the package name is spelled correctly"
    end
  end

  describe "403 errors (permission denied) - unauthenticated" do
    setup do
      # Clear any authentication
      Hex.State.put(:oauth_token, nil)
      Hex.State.put(:repos_key, nil)
      Hex.State.put(:api_key, nil)
      :ok
    end

    test "displays auth prompt for private package" do
      package_name = "private_package"
      result = {:ok, {403, [], %{}}}

      Hex.Registry.Server.print_missing_package_diagnostics(
        "hexpm",
        package_name,
        result
      )

      assert_received {:mix_shell, :error, [msg]}
      assert msg =~ "You don't have permission to access #{package_name}"
      assert msg =~ "the package is private and requires authentication"
      assert msg =~ "run 'mix hex.user auth' to authenticate"
    end
  end

  describe "403 errors (permission denied) - authenticated" do
    setup do
      # Simulate authenticated user with OAuth token
      token_data = %{
        "access_token" => "test_token",
        "refresh_token" => "refresh_token",
        "expires_at" => System.system_time(:second) + 3600
      }

      Hex.State.put(:oauth_token, token_data)
      :ok
    end

    test "displays permission diagnostics for private package" do
      package_name = "private_package"
      result = {:ok, {403, [], %{}}}

      Hex.Registry.Server.print_missing_package_diagnostics(
        "hexpm",
        package_name,
        result
      )

      assert_received {:mix_shell, :error, [msg]}
      assert msg =~ "You don't have permission to access #{package_name}"
      assert msg =~ "the package is private and you don't have the required permissions"
      assert msg =~ "Contact the package owner to request access"
      assert msg =~ "https://hex.pm/dashboard"
    end
  end

  defp send_error_message(result, repo, package, cached?) do
    package_name = Hex.Utils.package_name(repo, package)
    cached_message = if cached?, do: " (using cache instead)", else: ""

    Hex.Shell.error("Failed to fetch record for #{package_name} from registry#{cached_message}")
    Hex.Registry.Server.print_missing_package_diagnostics(repo, package, result)
  end
end
