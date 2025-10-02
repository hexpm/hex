defmodule HexTest.Case do
  use ExUnit.CaseTemplate

  using do
    quote do
      import unquote(__MODULE__)
      alias HexTest.Case
      alias HexTest.Hexpm
    end
  end

  def flush do
    flush([])
  end

  defp flush(acc) do
    receive do
      any -> flush([any | acc])
    after
      0 -> Enum.reverse(acc)
    end
  end

  def tmp_path() do
    Path.expand("../../tmp", __DIR__)
  end

  def tmp_path(extension) do
    Path.join(tmp_path(), extension)
  end

  def fixture_path() do
    Path.expand("../fixtures", __DIR__)
  end

  def fixture_path(extension) do
    Path.join(fixture_path(), extension)
  end

  defp escape_path(path) do
    case :os.type() do
      {:win32, _} -> String.replace(path, ~r'[~#%&*{}\\:<>?/+|"]', "_")
      _ -> path
    end
  end

  defmacro test_tmp() do
    module = escape_path("#{__CALLER__.module}")
    function = escape_path("#{elem(__CALLER__.function || {nil}, 0)}")

    Path.join([HexTest.Case.tmp_path(), module, function])
  end

  defmacro test_name() do
    module = escape_path("#{__CALLER__.module}")
    function = escape_path("#{elem(__CALLER__.function || {nil}, 0)}")

    Path.join([module, function])
  end

  defmacro in_tmp(fun) do
    module = escape_path("#{__CALLER__.module}")
    function = escape_path("#{elem(__CALLER__.function || {nil}, 0)}")

    path = Path.join([module, function])

    quote do
      in_tmp(unquote(path), unquote(fun))
    end
  end

  def in_tmp(tmp, function) do
    path = tmp_path(tmp)
    File.rm_rf!(path)
    File.mkdir_p!(path)
    File.cd!(path, function)
  end

  defmacro in_fixture(which, block) do
    module = inspect(__CALLER__.module)
    function = Atom.to_string(elem(__CALLER__.function || {nil}, 0))
    tmp = Path.join(module, function)

    quote do
      unquote(__MODULE__).in_fixture(unquote(which), unquote(tmp), unquote(block))
    end
  end

  def in_fixture(which, tmp, function) do
    src = fixture_path(which)
    dest = tmp_path(String.replace(tmp, ":", "_"))
    flag = String.to_charlist(tmp_path())

    File.rm_rf!(dest)
    File.mkdir_p!(dest)
    File.cp_r!(src, dest)

    get_path = :code.get_path()
    previous = :code.all_loaded()

    try do
      File.cd!(dest, function)
    after
      :code.set_path(get_path)

      for {mod, file} <- :code.all_loaded() -- previous,
          file == [] or (is_list(file) and :lists.prefix(flag, file)) do
        purge([mod])
      end
    end
  end

  def purge(modules) do
    Enum.each(modules, fn m ->
      :code.delete(m)
      :code.purge(m)
    end)
  end

  @ets_table :hex_index

  def create_test_registry(path, registry) do
    registry = Enum.sort(registry)

    versions =
      Enum.reduce(registry, %{}, fn {repo, name, vsn, _deps}, map ->
        key = {Atom.to_string(repo), Atom.to_string(name)}
        Map.update(map, key, [vsn], &(&1 ++ [vsn]))
      end)
      |> Enum.to_list()

    deps =
      Enum.map(registry, fn {outer_repo, name, vsn, deps} ->
        deps =
          Enum.map(deps, fn config ->
            destructure [name, req, optional, app, repo], Tuple.to_list(config)
            optional = optional || false
            app = app || name
            repo = repo || outer_repo
            {Atom.to_string(repo), Atom.to_string(name), Atom.to_string(app), req, optional}
          end)

        {{Atom.to_string(outer_repo), Atom.to_string(name), vsn}, deps}
      end)

    create_registry(path, versions, deps)
  end

  defp create_registry(path, versions, deps) do
    tid = :ets.new(@ets_table, [])

    versions =
      Enum.map(versions, fn {{repo, pkg}, versions} ->
        {{:versions, repo, pkg}, versions}
      end)

    deps =
      Enum.map(deps, fn {{repo, pkg, vsn}, deps} ->
        {{:deps, repo, pkg, vsn}, deps}
      end)

    :ets.insert(tid, versions ++ deps ++ [{:version, 3}])
    File.mkdir_p!(Path.dirname(path))
    :ok = :ets.tab2file(tid, String.to_charlist(path))
    :ets.delete(tid)
  end

  defp test_registry() do
    [
      {:hexpm, :bar, "0.0.1", []},
      {:hexpm, :bar, "0.1.0", [foo: "~> 0.1.0"]},
      {:hexpm, :bar, "0.2.0", [foo: "~> 0.2.0"]},
      {:hexpm, :beta, "1.0.0", []},
      {:hexpm, :beta, "1.1.0-beta", []},
      {:hexpm, :decimal, "0.0.1", []},
      {:hexpm, :decimal, "0.1.0", []},
      {:hexpm, :decimal, "0.2.0", []},
      {:hexpm, :decimal, "0.2.1", []},
      {:hexpm, :depend_name, "0.2.0", [{:package_name, ">= 0.0.0", false, :app_name}]},
      {:hexpm, :ecto, "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"]},
      {:hexpm, :ecto, "0.2.1", [postgrex: "~> 0.2.1", ex_doc: "0.1.0"]},
      {:hexpm, :ecto, "1.1.0", [poison: "~> 1.0"]},
      {:hexpm, :eric, "0.0.1", []},
      {:hexpm, :eric, "0.0.2", []},
      {:hexpm, :eric, "0.1.0", [jose: "~> 0.1.0"]},
      {:hexpm, :eric, "0.1.2", [jose: "~> 0.1.0"]},
      {:hexpm, :eric, "0.2.0", [jose: "~> 0.3.0"]},
      {:hexpm, :ex_doc, "0.0.1", []},
      {:hexpm, :ex_doc, "0.0.2", []},
      {:hexpm, :ex_doc, "0.1.0", []},
      {:hexpm, :ex_plex, "0.0.1", []},
      {:hexpm, :ex_plex, "0.0.2", [decimal: "0.1.1"]},
      {:hexpm, :ex_plex, "0.1.0", [decimal: "~> 0.1.0"]},
      {:hexpm, :ex_plex, "0.1.2", [decimal: "~> 0.1.0"]},
      {:hexpm, :ex_plex, "0.2.0", [decimal: "~> 0.2.0"]},
      {:hexpm, :foo, "0.0.1", []},
      {:hexpm, :foo, "0.1.0", []},
      {:hexpm, :foo, "0.2.0", []},
      {:hexpm, :foo, "0.2.1", []},
      {:hexpm, :has_optional, "0.1.0", [{:ex_doc, "~> 0.0.1", true}]},
      {:hexpm, :invalid_dirname, "0.1.0", []},
      {:hexpm, :invalid_filename, "0.1.0", []},
      {:hexpm, :jose, "0.2.0", []},
      {:hexpm, :jose, "0.2.1", []},
      {:hexpm, :only_doc, "0.1.0", [{:ex_doc, ">= 0.0.0", true}]},
      {:hexpm, :package_name, "0.1.0", []},
      {:hexpm, :phoenix, "0.0.1", [postgrex: "~> 0.2"]},
      {:hexpm, :phoenix, "1.1.2", [poison: "~> 1.5 or ~> 2.0"]},
      {:hexpm, :phoenix, "1.1.3", [poison: "~> 1.5 or ~> 2.0"]},
      {:hexpm, :poison, "1.5.2", []},
      {:hexpm, :poison, "2.0.0", []},
      {:hexpm, :phoenix_ecto, "2.0.0", [ecto: "~> 1.1", poison: "~> 1.3"]},
      {:hexpm, :phoenix_ecto, "2.0.1", [ecto: "~> 1.1", poison: "~> 1.3"]},
      {:hexpm, :phoenix_live_reload, "1.0.0", [phoenix: "~> 0.16 or ~> 1.0"]},
      {:hexpm, :phoenix_live_reload, "1.0.3", [phoenix: "~> 0.16 or ~> 1.0"]},
      {:hexpm, :postgrex, "0.2.0", [ex_doc: "0.0.1"]},
      {:hexpm, :postgrex, "0.2.1", [ex_doc: "~> 0.1.0"]},
      {:hexpm, :umb, "0.2.1", [ex_doc: "~> 0.1.0"]},
      {:repo2, :hexpm_deps, "0.1.0", [{:poison, ">= 0.0.0", false, :poison, :hexpm}]},
      {:repo2, :poison, "2.0.0", []},
      {:repo2, :repo2_deps, "0.1.0", [poison: ">= 0.0.0"]}
    ]
  end

  def setup_auth(username, password) do
    write_permissions = [%{"domain" => "api"}]
    read_permissions = [%{"domain" => "api", "resource" => "read"}]

    {:ok, {201, _, write_body}} =
      Hex.API.Key.new("setup_auth_write", write_permissions, user: username, pass: password)

    {:ok, {201, _, _read_body}} =
      Hex.API.Key.new("setup_auth_read", read_permissions, user: username, pass: password)

    write_key = write_body["secret"]
    Hex.State.put(:api_key, write_key)
    [key: write_key]
  end

  def get_auth(username, password) do
    permissions = [%{"domain" => "api"}]

    {:ok, {201, _, body}} =
      Hex.API.Key.new("setup_auth", permissions, user: username, pass: password)

    [key: body["secret"]]
  end

  def init_reset_state() do
    public_key = File.read!(Path.join(__DIR__, "../fixtures/test_pub.pem"))

    Hex.State.put(:config_home, Path.expand("../../tmp/hex_config_home", __DIR__))
    Hex.State.put(:cache_home, Path.expand("../../tmp/hex_cache_home", __DIR__))
    Hex.State.put(:data_home, Path.expand("../../tmp/hex_data_home", __DIR__))
    Hex.State.put(:api_url, "http://localhost:4043/api")
    Hex.State.put(:api_key, nil)
    Hex.State.put(:oauth_tokens, nil)
    Hex.State.update!(:repos, &put_in(&1["hexpm"].url, "http://localhost:4043/repo"))
    Hex.State.update!(:repos, &put_in(&1["hexpm"].public_key, public_key))
    Hex.State.update!(:repos, &put_in(&1["hexpm"].auth_key, nil))
    Hex.State.put(:repos_key, nil)
    Hex.State.put(:pbkdf2_iters, 10)
    Hex.State.put(:clean_pass, false)
    Application.put_env(:hex, :reset_state, Hex.State.get_all())
  end

  def reset_state do
    Hex.State.put_all(Application.get_env(:hex, :reset_state))
  end

  def set_home_cwd() do
    set_home_path(File.cwd!())
  end

  def set_home_tmp() do
    set_home_path(tmp_path())
  end

  def set_home_path(path) do
    Hex.State.put(:config_home, path)
    Hex.State.put(:cache_home, path)
    Hex.State.put(:data_home, path)
  end

  def wait_on_exit({:ok, pid}) do
    on_exit(fn ->
      ref = Process.monitor(pid)

      receive do
        {:DOWN, ^ref, :process, ^pid, _info} ->
          :ok
      end
    end)
  end

  setup_all context do
    unless context[:async] do
      ets_path = tmp_path("cache.ets")
      File.rm(ets_path)
      create_test_registry(ets_path, test_registry())
      reset_state()
    end

    :ok
  end

  setup context do
    unless context[:async] do
      wait_on_exit(Hex.Registry.Server.start_link())
      wait_on_exit(Hex.UpdateChecker.start_link())

      reset_state()
      Hex.Parallel.clear(:hex_fetcher)
      Mix.shell(Hex.Shell.Process)
      Mix.Task.clear()
      Hex.Shell.Process.flush()
      Mix.State.clear_cache()
      Mix.ProjectStack.clear_stack()
    end

    :ok
  end

  def bypass_mirror() do
    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)
    repos = put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}")
    Hex.State.put(:repos, repos)

    Bypass.expect(bypass, fn conn ->
      case conn do
        %Plug.Conn{request_path: "/docs/docs_package-1.1.2.tar.gz"} ->
          tar_file = tmp_path("docs_package-1.1.2.tar.gz")
          index_file = String.to_charlist("index.html")
          epub_file = String.to_charlist("docs_package.epub")
          :mix_hex_erl_tar.create(tar_file, [{index_file, ""}, {epub_file, ""}], [:compressed])
          package = File.read!(tar_file)
          Plug.Conn.resp(conn, 200, package)

        %Plug.Conn{request_path: "/docs/docs_package"} ->
          Plug.Conn.resp(conn, 404, "")

        %Plug.Conn{request_path: "/docs/pre_only_package-0.0.1-rc1.tar.gz"} ->
          tar_file = tmp_path("pre_only_package-0.0.1-rc1.tar.gz")
          index_file = String.to_charlist("index.html")
          :mix_hex_erl_tar.create(tar_file, [{index_file, ""}], [:compressed])
          package = File.read!(tar_file)
          Plug.Conn.resp(conn, 200, package)
      end
    end)

    bypass
  end

  def bypass_repo(repo) do
    bypass = Bypass.open()

    map = %{
      url: "http://localhost:#{bypass.port}/repo",
      public_key: nil,
      auth_key: nil,
      organization: "hexpm"
    }

    repos = Hex.State.fetch!(:repos)
    repos = Map.put(repos, repo, map)
    Hex.State.put(:repos, repos)
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    package_path = "/api/repos/#{repo}/packages/ecto"
    release_path = "/api/repos/#{repo}/publish"

    Bypass.expect(bypass, fn conn ->
      case conn do
        %Plug.Conn{method: "GET", request_path: ^package_path} ->
          body = %{"meta" => %{"description" => "ecto description"}}

          conn
          |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
          |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(body))

        %Plug.Conn{method: "POST", request_path: ^release_path} ->
          body = %{"html_url" => "myrepo html_url"}

          conn
          |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
          |> Plug.Conn.resp(201, Hex.Utils.safe_serialize_erlang(body))

        %Plug.Conn{method: "GET", request_path: "/api/users/me"} ->
          body = %{"organizations" => [%{"name" => repo}]}

          conn
          |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
          |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(body))

        %Plug.Conn{method: "POST", request_path: "/api/keys"} ->
          body = %{"secret" => "myrepo secret"}

          conn
          |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
          |> Plug.Conn.resp(201, Hex.Utils.safe_serialize_erlang(body))
      end
    end)

    bypass
  end

  def bypass_package() do
    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)
    repos = put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}")
    Hex.State.put(:repos, repos)

    Bypass.expect(bypass, fn conn ->
      case conn do
        %Plug.Conn{request_path: "/tarballs/package-1.0.0.tar"} ->
          tar_file = tmp_path("package-1.0.0.tar")
          index_file = String.to_charlist("index.html")
          :mix_hex_erl_tar.create(tar_file, [{index_file, ""}], [:compressed])
          package = File.read!(tar_file)
          Plug.Conn.resp(conn, 200, package)

        %Plug.Conn{request_path: "/tarballs/package-1.0.1.tar"} ->
          Plug.Conn.resp(conn, 404, "")
      end
    end)

    bypass
  end
end
