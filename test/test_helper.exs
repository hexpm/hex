ExUnit.start exclude: [:integration]

defmodule HexTest.Case do
  use ExUnit.CaseTemplate

  def tmp_path do
    Path.expand("../tmp", __DIR__)
  end

  def tmp_path(extension) do
    Path.join(tmp_path, extension)
  end

  def fixture_path do
    Path.expand("fixtures", __DIR__)
  end

  def fixture_path(extension) do
    Path.join(fixture_path, extension)
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

  @template """
  defmodule ~s.NoConflict.Mixfile do
    use Mix.Project

    def project do
      [ app: :~s,
        version: "~s",
        deps: deps() ]
    end

    defp deps do
      ~s
    end
  end
  """

  def init_project(name, version, deps, meta, auth) do
    reqs =
      Enum.filter(deps, fn
        {_app, _req, opts} -> !(opts[:path] || opts[:git] || opts[:github])
        _ -> true
      end)

    reqs = Enum.into(reqs, %{}, fn
      {app, req} ->
        {app, %{app: app, requirement: req, optional: false}}
      {app, req, opts} ->
        default_opts = %{app: app, requirement: req, optional: false}
        {opts[:hex] || app, Dict.merge(default_opts, opts)}
    end)

    meta = Map.merge(meta, %{name: name, version: version, requirements: reqs})
    meta = Map.put_new(meta, :app, name)

    deps = inspect(deps, pretty: true)
    module = String.capitalize(name)
    mixfile = :io_lib.format(@template, [module, name, version, deps])

    files = [{"mix.exs", List.to_string(mixfile)}]
    tar = Hex.Tar.create(meta, files)

    {_, %{"name" => ^name}}       = Hex.API.Package.new(name, meta, auth)
    {_, %{"version" => ^version}} = Hex.API.Release.new(name, tar, auth)
  end

  def purge(modules) do
    Enum.each modules, fn(m) ->
      :code.delete(m)
      :code.purge(m)
    end
  end

  @ets_table :hex_ets_registry
  @version   4

  def create_test_registry(path) do
    packages =
      Enum.reduce(test_registry, HashDict.new, fn {name, vsn, _}, dict ->
        Dict.update(dict, "#{name}", [vsn], &[vsn|&1])
      end)

    packages =
      Enum.map(packages, fn {name, vsns} ->
        {"#{name}", [Enum.sort(vsns, &(Version.compare(&1, &2) == :lt))]}
      end)

    releases =
      Enum.map(test_registry, fn {name, version, deps} ->
        deps = Enum.map(deps, fn
          {name, req} -> ["#{name}", req, false, "#{name}"]
          {name, req, optional} -> ["#{name}", req, optional, "#{name}"]
          {name, req, optional, app} -> ["#{name}", req, optional, "#{app}"]
        end)
        {{"#{name}", version}, [deps, nil]}
      end)

    create_registry(path, @version, [], releases, packages)
  end

  def create_registry(path, version, installs, releases, packages) do
    tid = :ets.new(@ets_table, [])
    :ets.insert(tid, {:"$$version$$", version})
    :ets.insert(tid, {:"$$installs2$$", installs})
    :ets.insert(tid, releases ++ packages)
    :ok = :ets.tab2file(tid, String.to_char_list(path))
    :ets.delete(tid)
  end

  defp test_registry do
    [ {:foo, "0.0.1", []},
      {:foo, "0.1.0", []},
      {:foo, "0.2.0", []},
      {:foo, "0.2.1", []},
      {:bar, "0.0.1", []},
      {:bar, "0.1.0", [foo: "~> 0.1.0"]},
      {:bar, "0.2.0", [foo: "~> 0.2.0"]},

      {:decimal, "0.0.1", []},
      {:decimal, "0.1.0", []},
      {:decimal, "0.2.0", []},
      {:decimal, "0.2.1", []},
      {:ex_plex, "0.0.1", []},
      {:ex_plex, "0.0.2", [decimal: "0.1.1"]},
      {:ex_plex, "0.1.0", [decimal: "~> 0.1.0"]},
      {:ex_plex, "0.1.2", [decimal: "~> 0.1.0"]},
      {:ex_plex, "0.2.0", [decimal: "~> 0.2.0"]},

      {:jose, "0.2.0", []},
      {:jose, "0.2.1", []},
      {:eric, "0.0.1", []},
      {:eric, "0.0.2", []},
      {:eric, "0.1.0", [jose: "~> 0.1.0"]},
      {:eric, "0.1.2", [jose: "~> 0.1.0"]},
      {:eric, "0.2.0", [jose: "~> 0.3.0"]},

      {:ex_doc, "0.0.1", []},
      {:ex_doc, "0.0.2", []},
      {:ex_doc, "0.1.0", []},
      {:postgrex, "0.2.0", [ex_doc: "0.0.1"]},
      {:postgrex, "0.2.1", [ex_doc: "~> 0.1.0"]},
      {:ecto, "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"]},
      {:ecto, "0.2.1", [postgrex: "~> 0.2.1", ex_doc: "0.1.0"]},

      {:only_doc, "0.1.0", [{:ex_doc, nil, true}]},

      {:ex_doc_parent, "0.1.0", [ex_doc: "~> 0.1.0"]},

      {:package_name, "0.1.0", []},
      {:depend_name, "0.2.0", [{:package_name, nil, false, :app_name}]} ]
  end

  def setup_auth(username) do
    user = HexWeb.User.get(username: username)
    key  = :crypto.rand_bytes(4) |> Base.encode16

    {:ok, key} = HexWeb.API.Key.create(key, user)
    Hex.Config.update([key: key.user_secret])
  end

  using do
    quote do
      import unquote(__MODULE__)
    end
  end

  setup context do
    # Slow down tests to work around race condition in httpc that occurs during
    # heavy load. Useful when running CI.
    if context[:integration] && System.get_env("SLOW_HTTP") == "true" do
      :timer.sleep(1000)
    end

    :ok
  end

  setup_all do
    ets_path = tmp_path("registry.ets")
    File.rm(ets_path)
    create_test_registry(ets_path)
    :ok
  end

  @registry_tid :registry_tid

  setup do
    Hex.home(tmp_path("hex_home"))
    Hex.Parallel.clear(:hex_fetcher)
    Mix.shell(Mix.Shell.Process)
    Mix.Task.clear
    Mix.Shell.Process.flush
    Mix.ProjectStack.clear_cache
    Mix.ProjectStack.clear_stack
    Application.put_env(:hex, :registry_updated, true)
    Application.delete_env(:hex, @registry_tid)

    :ok
  end
end

alias HexTest.Case
File.rm_rf!(Case.tmp_path)
File.mkdir_p!(Case.tmp_path)


if :integration in ExUnit.configuration[:include] do
  db = "hex_test"
  db_url = "ecto://postgres:postgres@localhost/#{db}"

  System.put_env("DATABASE_URL", db_url)

  File.cd! "_build/test/lib/hex_web", fn ->
    Mix.Task.run "ecto.drop", ["HexWeb.Repo"]
    Mix.Task.run "ecto.create", ["HexWeb.Repo"]
    Mix.Task.run "ecto.migrate", ["HexWeb.Repo"]
  end
  HexWeb.Repo.stop

  {:ok, _} = Application.ensure_all_started(:hex_web)

  Hex.start()
  Hex.url("http://localhost:4000")
  unless System.get_env("HEX_CDN"), do: Hex.cdn(Hex.url)

  meta = %{
    "contributors" => ["John Doe", "Jane Doe"],
    "licenses" => ["GPL2", "MIT", "Apache"],
    "links" => %{"docs" => "http://docs", "repo" => "http://repo"},
    "description" => "builds docs"}

  {:ok, user}    = HexWeb.User.create("user", "user@mail.com", "hunter42", true)
  {:ok, package} = HexWeb.Package.create("ex_doc", user, meta)
  {:ok, _}       = HexWeb.Release.create(package, "0.0.1", "ex_doc", %{}, "")

  {201, %{"secret" => secret}} = Hex.API.Key.new("my_key", [user: "user", pass: "hunter42"])
  auth = [key: secret]

  Case.init_project("ex_doc", "0.0.1", [], meta, auth)
  Case.init_project("ex_doc", "0.0.1", [], meta, auth)
  Case.init_project("ex_doc", "0.1.0", [], meta, auth)
  Case.init_project("ex_doc_parent", "0.1.0", [], meta, auth)
  Case.init_project("postgrex", "0.2.1", [ex_doc: "~> 0.1.0"], %{}, auth)
  Case.init_project("postgrex", "0.2.0", [ex_doc: "0.0.1"], %{}, auth)
  Case.init_project("ecto", "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"], %{}, auth)
  Case.init_project("ecto", "0.2.1", [{:sample, "0.0.1", path: Case.fixture_path("sample")}, postgrex: "~> 0.2.1", ex_doc: "0.1.0"], %{}, auth)
  Case.init_project("only_doc", "0.1.0", [{:ex_doc, nil, optional: true}], %{}, auth)
  Case.init_project("ex_doc_parent", "0.1.0", [ex_doc: "~> 0.1.0"], meta, auth)
  Case.init_project("package_name", "0.1.0", [], %{app: "app_name"}, auth)
  Case.init_project("depend_name", "0.2.0", [{:app_name, nil, optional: true, hex: :package_name}], %{}, auth)
end

Mix.SCM.append(Hex.SCM)
Mix.RemoteConverger.register(Hex.RemoteConverger)
