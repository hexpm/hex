defmodule Hex.TerminalOutputTest do
  use HexTest.Case

  defmodule Project do
    def project, do: [app: :terminal_audit, version: "1.0.0", deps: [{:parent, "1.0.0"}]]
  end

  defmodule EmptyProject do
    def project, do: [app: :terminal_audit, version: "1.0.0"]
  end

  defp api do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    Hex.State.put(:api_key, "test-key")
    bypass
  end

  defp reply(conn, status, body) do
    conn
    |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, Hex.Utils.safe_serialize_erlang(body))
  end

  defp policy do
    %{
      repository: "private",
      name: "strict\e[2K\rFORGED",
      visibility: :VISIBILITY_PUBLIC,
      repositories: [%{repository: "hexpm", restriction: %{cooldown: "14d"}, overrides: []}]
    }
  end

  test "policy show escapes effective cooldown identity" do
    Hex.State.put(:active_policy, policy())
    Mix.Tasks.Hex.Policy.run(["show"])
    output = shell_output()
    assert output =~ "\\e[2K\\rFORGED"
    refute output =~ "\e[2K"
  end

  test "policy resolution summary escapes effective cooldown identity" do
    output = Hex.Policy.Diagnostics.resolution_summary(policy(), [], "0d")
    assert output =~ "Effective cooldown: 14d (private/strict\\e[2K\\rFORGED)"
    refute output =~ "\e[2K"
  end

  test "policy show escapes cooldown restriction" do
    p =
      put_in(policy(), [:repositories], [
        %{repository: "hexpm", restriction: %{cooldown: "14d\e[2K\rFORGED"}, overrides: []}
      ])

    Hex.State.put(:active_policy, %{p | name: "strict"})
    Mix.Tasks.Hex.Policy.run(["show"])
    output = shell_output()
    assert output =~ "\\e[2K\\rFORGED"
    refute output =~ "\e[2K"
  end

  test "organization key generation escapes the returned secret" do
    bypass = api()

    Bypass.expect(bypass, "POST", "/api/orgs/private/keys", fn conn ->
      reply(conn, 201, %{"secret" => "secret\e[2K\rFORGED"})
    end)

    Mix.Tasks.Hex.Organization.run(["key", "private", "generate"])
    output = shell_output()
    assert output =~ "\\e[2K\\rFORGED"
    refute output =~ "\e[2K"
  end

  test "publish escapes organization choices and selected owner" do
    in_tmp(fn ->
      set_home_tmp()
      Process.put(:hex_test_app_name, :terminal_audit_publish)
      Mix.Project.push(ReleaseSimple.MixProject)
      File.write!("myfile.txt", "hello")
      bypass = api()

      Bypass.expect(bypass, "GET", "/api/users/me", fn conn ->
        reply(conn, 200, %{"organizations" => [%{"name" => "private\e[2K\rFORGED"}]})
      end)

      Bypass.expect(bypass, "GET", "/api/repos/hexpm/packages/terminal_audit_publish", fn conn ->
        reply(conn, 404, %{})
      end)

      send(self(), {:mix_shell_input, :prompt, "2"})
      Mix.Tasks.Hex.Publish.run(["package", "--dry-run", "--no-progress"])
      output = shell_output()
      assert output =~ "[2] private\\e[2K\\rFORGED"
      assert output =~ "Transferring ownership to private\\e[2K\\rFORGED..."
      refute output =~ "\e[2K"
    end)
  end

  test "user auth drops terminal controls from the verification URI and user code" do
    in_tmp(fn ->
      set_home_tmp()
      bypass = Bypass.open()
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      Bypass.expect_once(bypass, "POST", "/api/oauth/device_authorization", fn conn ->
        reply(conn, 200, %{
          "device_code" => "device",
          "user_code" => "ABC\e[2K\rD",
          "verification_uri" => "ftp://hex.pm/device\e[2K\rFORGED",
          "verification_uri_complete" => "ftp://hex.pm/device\e[2K\rFORGED",
          "expires_in" => 600,
          "interval" => 0
        })
      end)

      Bypass.expect_once(bypass, "POST", "/api/oauth/token", fn conn ->
        reply(conn, 400, %{"error" => "access_denied"})
      end)

      # hex_core only opens http and https URIs in a browser
      Mix.Tasks.Hex.User.run(["auth"])

      output = shell_output()
      assert output =~ "Authentication was denied."
      assert output =~ "To authenticate, visit: ftp://hex.pm/device[2KFORGED"
      assert output =~ "  ABC-[2KD"
      refute output =~ "\e[2K"
      refute output =~ "\r"
    end)
  end

  test "dependency resolution rejects a registry dependency with a forged name" do
    in_tmp(fn ->
      Mix.Project.push(Project)
      path = tmp_path("terminal_audit_registry.ets")

      create_test_registry(path, [
        {:hexpm, :parent, "1.0.0", [{:"child\e[2K\rFORGED", "2.0.0"}]},
        {:hexpm, :"child\e[2K\rFORGED", "1.0.0", []}
      ])

      Hex.Registry.Server.close()
      Hex.State.put(:offline, true)
      Hex.Registry.Server.open(registry_path: path)
      Hex.State.put(:debug_solver, true)

      logs =
        ExUnit.CaptureLog.capture_log(fn ->
          error = assert_raise Mix.Error, fn -> Mix.Tasks.Deps.Get.run([]) end

          assert error.message ==
                   "Package parent 1.0.0 declares a dependency with an invalid package name " <>
                     ~s("child\\e[2K\\rFORGED")
        end)

      refute logs =~ "\e[2K"
      refute shell_output() =~ "\e[2K"
    end)
  end

  test "tar unpack failure escapes the archive entry name" do
    in_tmp(fn ->
      :ok = :mix_hex_erl_tar.create(~c"malicious.tar", [{~c"../bad\e[2K\rFORGED", "x"}], [])

      error =
        assert_raise Mix.Error, fn ->
          Hex.Tar.unpack!("malicious.tar", "output")
        end

      assert error.message =~ "../bad\\e[2K\\rFORGED"
      refute error.message =~ "\e[2K"
    end)
  end

  test "policy diagnostics escape filtered package names in summaries and failure notes" do
    entries =
      for minor <- 0..5 do
        %{
          repo: "hexpm",
          package: "child\e[2K\rFORGED",
          version: "1.#{minor}.0",
          reasons: [:override_deny]
        }
      end

    for output <- [
          Hex.Policy.Diagnostics.resolution_summary(policy(), entries, "0d"),
          Hex.Policy.Diagnostics.failure_note(entries)
        ] do
      assert output =~ "child\\e[2K\\rFORGED"
      assert output =~ "mix hex.policy why child\\e[2K\\rFORGED"
      refute output =~ "\e[2K"
    end
  end

  test "cooldown output escapes filtered package names" do
    now = System.system_time(:second)
    Hex.State.put(:cooldown_cutoff, {:cutoff, now - 86_400, 86_400})
    Hex.State.put(:cooldown_filtered_versions, [{"hexpm", "child\e[2K\rFORGED", "1.0.0", now}])
    Hex.RemoteConverger.print_cooldown_summary(%{})
    output = shell_output()
    assert output =~ "child\\e[2K\\rFORGED 1.0.0"
    refute output =~ "\e[2K"
  end

  test "offline registry errors escape package and policy names" do
    Hex.State.put(:offline, true)
    Hex.Registry.Server.open(registry_path: tmp_path("cache.ets"))

    for fetch <- [
          fn -> Hex.Registry.Server.prefetch([{"hexpm", "child\e[2K\rFORGED"}]) end,
          fn -> Hex.Registry.Server.prefetch_policies([{"hexpm", "strict\e[2K\rFORGED"}]) end
        ] do
      error = assert_raise Mix.Error, fetch
      assert error.message =~ "\\e[2K\\rFORGED"
      refute error.message =~ "\e[2K"
    end
  end

  test "tar creation errors escape rejected paths" do
    error =
      assert_raise Mix.Error, fn ->
        Hex.Tar.create!(
          %{name: "parent", version: "1.0.0"},
          [{"../bad\e[2K\rFORGED", "x"}],
          :memory
        )
      end

    assert error.message =~ "../bad\\e[2K\\rFORGED"
    refute error.message =~ "\e[2K"
  end

  test "dependency resolution rejects registry application names with control characters" do
    registries = [
      [
        {:hexpm, :parent, "1.0.0",
         [{:child, "1.0.0", false, :"app\e[2K\rFORGED"}, {:other, "1.0.0"}]},
        {:hexpm, :other, "1.0.0", [{:child, "1.0.0", false, :different}]},
        {:hexpm, :child, "1.0.0", []}
      ],
      [
        {:hexpm, :parent, "1.0.0",
         [
           {:child, "1.0.0", false, :"app\e[2K\rFORGED"},
           {:other, "1.0.0", false, :"app\e[2K\rFORGED"}
         ]},
        {:hexpm, :other, "1.0.0", []},
        {:hexpm, :child, "1.0.0", []}
      ]
    ]

    in_tmp(fn ->
      Mix.Project.push(Project)

      for registry <- registries do
        path = tmp_path("terminal_conflict.ets")
        Hex.Registry.Server.close()
        create_test_registry(path, registry)
        Hex.State.put(:offline, true)
        Hex.State.put(:debug_solver, true)
        Hex.Registry.Server.open(registry_path: path)
        Mix.Task.clear()

        logs =
          ExUnit.CaptureLog.capture_log(fn ->
            assert_raise Mix.Error, "Hex dependency resolution failed", fn ->
              Mix.Tasks.Deps.Get.run([])
            end
          end)

        output = shell_output()
        assert output =~ ~s(has the invalid application name "app\\e[2K\\rFORGED")
        refute output =~ "\e[2K"
        refute output =~ "\r"
        refute logs =~ "\e[2K"
      end
    end)
  end
end
