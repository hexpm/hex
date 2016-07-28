defmodule HexTest.Case do
  use ExUnit.CaseTemplate

  using do
    quote do
      import unquote(__MODULE__)
      alias HexTest.Case
      alias HexTest.HexWeb
    end
  end

  def flush do
    flush([])
  end

  defp flush(acc) do
    receive do
      any -> flush([any|acc])
    after
      0 -> Enum.reverse(acc)
    end
  end

  def tmp_path do
    Path.expand("../../tmp", __DIR__)
  end

  def tmp_path(extension) do
    Path.join(tmp_path(), extension)
  end

  def fixture_path do
    Path.expand("../fixtures", __DIR__)
  end

  def fixture_path(extension) do
    Path.join(fixture_path(), extension)
  end

  defmacro in_tmp(fun) do
    path = Path.join(["#{__CALLER__.module}", "#{elem(__CALLER__.function, 0)}"])
    quote do
      in_tmp(unquote(path), unquote(fun))
    end
  end

  defmacro in_fixture(which, fun) do
    path = Path.join(["#{__CALLER__.module}", "#{elem(__CALLER__.function, 0)}"])
    quote do
      in_fixture(unquote(which), unquote(path), unquote(fun))
    end
  end

  def in_tmp(tmp, function) do
    path = tmp_path(tmp)
    File.rm_rf!(path)
    File.mkdir_p!(path)
    File.cd!(path, function)
  end

  def in_fixture(which, tmp, function) do
    tmp = tmp_path(tmp)
    File.rm_rf!(tmp)
    File.mkdir_p!(tmp)

    which = fixture_path(which)
    File.cp_r!(which, tmp)

    File.cd!(tmp, function)
  end

  def purge(modules) do
    Enum.each modules, fn(m) ->
      :code.delete(m)
      :code.purge(m)
    end
  end

  @ets_table :hex_index

  def create_test_registry(path) do
    versions =
      Enum.reduce(test_registry(), %{}, fn {name, vsn, _}, dict ->
        Map.update(dict, Atom.to_string(name), [vsn], &(&1 ++ [vsn]))
      end)
      |> Enum.to_list

    deps =
      Enum.map(test_registry(), fn {name, vsn, deps} ->
        deps = Enum.map(deps, fn
          {name, req} ->
            {Atom.to_string(name), Atom.to_string(name), req, false}
          {name, req, optional} ->
            {Atom.to_string(name), Atom.to_string(name), req, optional}
          {name, req, optional, app} ->
            {Atom.to_string(name), Atom.to_string(app), req, optional}
        end)
        {{Atom.to_string(name), vsn}, deps}
      end)

    create_registry(path, versions, deps)
  end

  defp create_registry(path, versions, deps) do
    tid = :ets.new(@ets_table, [])
    versions = Enum.map(versions, fn {pkg, val} -> {{:versions, pkg}, val} end)
    deps = Enum.map(deps, fn {{pkg, vsn}, val} -> {{:deps, pkg, vsn}, val} end)
    :ets.insert(tid, versions ++ deps)
    :ok = :ets.tab2file(tid, String.to_char_list(path))
    :ets.delete(tid)
  end

  # Needs to be sorted on names and versions
  defp test_registry do
    [{:bar, "0.0.1", []},
     {:bar, "0.1.0", [foo: "~> 0.1.0"]},
     {:bar, "0.2.0", [foo: "~> 0.2.0"]},
     {:beta, "1.0.0", []},
     {:beta, "1.1.0-beta", []},
     {:decimal, "0.0.1", []},
     {:decimal, "0.1.0", []},
     {:decimal, "0.2.0", []},
     {:decimal, "0.2.1", []},
     {:depend_name, "0.2.0", [{:package_name, ">= 0.0.0", false, :app_name}]},
     {:ecto, "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"]},
     {:ecto, "0.2.1", [postgrex: "~> 0.2.1", ex_doc: "0.1.0"]},
     {:ecto, "1.1.0", [poison: "~> 1.0"]},
     {:eric, "0.0.1", []},
     {:eric, "0.0.2", []},
     {:eric, "0.1.0", [jose: "~> 0.1.0"]},
     {:eric, "0.1.2", [jose: "~> 0.1.0"]},
     {:eric, "0.2.0", [jose: "~> 0.3.0"]},
     {:ex_doc, "0.0.1", []},
     {:ex_doc, "0.0.2", []},
     {:ex_doc, "0.1.0", []},
     {:ex_plex, "0.0.1", []},
     {:ex_plex, "0.0.2", [decimal: "0.1.1"]},
     {:ex_plex, "0.1.0", [decimal: "~> 0.1.0"]},
     {:ex_plex, "0.1.2", [decimal: "~> 0.1.0"]},
     {:ex_plex, "0.2.0", [decimal: "~> 0.2.0"]},
     {:foo, "0.0.1", []},
     {:foo, "0.1.0", []},
     {:foo, "0.2.0", []},
     {:foo, "0.2.1", []},
     {:has_optional, "0.1.0", [{:ex_doc, "~> 0.0.1", true}]},
     {:jose, "0.2.0", []},
     {:jose, "0.2.1", []},
     {:only_doc, "0.1.0", [{:ex_doc, ">= 0.0.0", true}]},
     {:package_name, "0.1.0", []},
     {:phoenix, "0.0.1", [postgrex: "~> 0.2"]},
     {:phoenix, "1.1.2", [poison: "~> 1.5 or ~> 2.0"]},
     {:phoenix, "1.1.3", [poison: "~> 1.5 or ~> 2.0"]},
     {:poison, "1.5.2", []},
     {:poison, "2.0.0", []},
     {:phoenix_ecto, "2.0.0", [ecto: "~> 1.1", poison: "~> 1.3"]},
     {:phoenix_ecto, "2.0.1", [ecto: "~> 1.1", poison: "~> 1.3"]},
     {:phoenix_live_reload, "1.0.0", [phoenix: "~> 0.16 or ~> 1.0"]},
     {:phoenix_live_reload, "1.0.3", [phoenix: "~> 0.16 or ~> 1.0"]},
     {:postgrex, "0.2.0", [ex_doc: "0.0.1"]},
     {:postgrex, "0.2.1", [ex_doc: "~> 0.1.0"]},]
  end

  def setup_auth(username, password) do
    {201, body, _} = Hex.API.Key.new("setup_auth", [user: username, pass: password])
    Mix.Hex.Utils.persist_key(password, body["secret"])
  end

  def get_auth(username, password) do
    {201, body, _} = Hex.API.Key.new("setup_auth", [user: username, pass: password])
    [key: body["secret"]]
  end

  {:ok, _} = Hex.State.start_link

  Hex.State.put(:home, Path.expand("../../tmp/hex_home", __DIR__))
  Hex.State.put(:hexpm_pk, File.read!(Path.join(__DIR__, "../fixtures/test_pub.pem")))
  Hex.State.put(:api, "http://localhost:4043/api")
  Hex.State.put(:mirror, System.get_env("HEX_MIRROR") || "http://localhost:4043/repo")
  Hex.State.put(:pbkdf2_iters, 10)
  Hex.State.put(:clean_pass, false)

  @hex_state Hex.State.get_all

  Hex.State.stop

  def reset_state do
    Hex.State.put_all(@hex_state)
  end

  setup_all do
    ets_path = tmp_path("cache.ets")
    File.rm(ets_path)
    create_test_registry(ets_path)
    :ok
  end

  setup do
    reset_state()

    Hex.Parallel.clear(:hex_fetcher)
    Hex.Registry.Server.close

    Mix.shell(Mix.Shell.Process)
    Mix.Task.clear
    Mix.Shell.Process.flush
    Mix.ProjectStack.clear_cache
    Mix.ProjectStack.clear_stack

    :ok
  end

  def bypass_mirror() do
    bypass = Bypass.open
    Hex.State.put(:repo, "http://localhost:#{bypass.port}")

    Bypass.expect bypass, fn conn ->
      case conn do
        %Plug.Conn{request_path: "/docs/package-1.1.2.tar.gz"} ->
          tar_file = tmp_path("package-1.1.2.tar.gz")
          index_file = String.to_char_list("index.html")
          :erl_tar.create(tar_file, [{index_file, ""}], [:compressed])
          package = File.read!(tar_file)
          Plug.Conn.resp(conn, 200, package)
        %Plug.Conn{request_path: "/docs/package"} ->
          Plug.Conn.resp(conn, 404, "")
      end
    end

    bypass
  end
end
