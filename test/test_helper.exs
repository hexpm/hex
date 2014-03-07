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
    path = Path.join([tmp_path, __CALLER__.module, elem(__CALLER__.function, 0)])
    quote do
      path = unquote(path)
      File.rm_rf!(path)
      File.mkdir_p!(path)
      File.cd!(path, fn -> unquote(fun).(path) end)
    end
  end

  def in_tmp(which, function) do
    path = tmp_path(which)
    File.rm_rf! path
    File.mkdir_p! path
    File.cd! path, function
  end

  defmacro in_fixture(which, block) do
    module   = inspect __CALLER__.module
    function = atom_to_binary elem(__CALLER__.function, 0)
    tmp      = Path.join(module, function)

    quote do
      unquote(__MODULE__).in_fixture(unquote(which), unquote(tmp), unquote(block))
    end
  end

  def in_fixture(which, tmp, function) do
    src  = fixture_path(which)
    dest = tmp_path(tmp)
    flag = String.to_char_list(tmp_path())

    File.rm_rf!(dest)
    File.mkdir_p!(dest)
    File.cp_r!(src, dest)

    get_path = :code.get_path
    previous = :code.all_loaded

    try do
      File.cd! dest, function
    after
      :code.set_path(get_path)
      Enum.each (:code.all_loaded -- previous), fn { mod, file } ->
        if is_list(file) and :lists.prefix(flag, file) do
          purge [mod]
        end
      end
    end
  end

  @template '''
  defmodule ~s.NoConflict.Mixfile do
    use Mix.Project

    def project do
      [ app: :~s,
        version: "~s",
        deps: deps() ]
    end

    def deps do
      ~s
    end
  end
  '''

  def init_fixture(name, version, deps) do
    path = fixture_path("#{name}-#{version}")
    File.rm_rf!(path)
    File.mkdir_p!(path)

    module = String.capitalize(name)
    deps = inspect(deps, pretty: true)
    mixfile = :io_lib.format(@template, [module, name, version, deps])

    File.cd! path, fn ->
      File.write(Path.join(path, "mix.exs"), mixfile)

      System.cmd("git init")
      System.cmd("git add .")
      System.cmd("git config user.email \"hex@example.com\"")
      System.cmd("git config user.name \"Hex Repo\"")
      System.cmd("git commit -m \"ok\"")
    end
  end

  def purge(modules) do
    Enum.each modules, fn(m) ->
      :code.delete(m)
      :code.purge(m)
    end
  end

  @ets_table :hex_ets_registry
  @version    1

  def create_test_registry(path) do
    packages =
      Enum.reduce(test_registry, HashDict.new, fn { name, vsn, _ }, dict ->
        Dict.update(dict, "#{name}", [vsn], &[vsn|&1])
      end)

    packages =
      Enum.map(packages, fn { name, vsns } ->
        { "#{name}", Enum.sort(vsns, &(Version.compare(&1, &2) == :lt)) }
      end)

    releases =
      Enum.map(test_registry, fn { name, version, deps } ->
        url  = fixture_path("#{name}-#{version}")
        deps = Enum.map(deps, fn { app, req } -> { "#{app}", req } end)
        { { "#{name}", version }, deps, url, "HEAD" }
      end)

    tid = :ets.new(@ets_table, [])
    :ets.insert(tid, { :"$$version$$", @version })
    :ets.insert(tid, releases ++ packages)
    :ok = :ets.tab2file(tid, String.to_char_list!(path))
    :ets.delete(tid)
  end

  defp test_registry do
    [ { :foo, "0.0.1", [] },
      { :foo, "0.1.0", [] },
      { :foo, "0.2.0", [] },
      { :foo, "0.2.1", [] },
      { :bar, "0.0.1", [] },
      { :bar, "0.1.0", [foo: "~> 0.1.0"] },
      { :bar, "0.2.0", [foo: "~> 0.2.0"] },

      { :decimal, "0.0.1", [] },
      { :decimal, "0.1.0", [] },
      { :decimal, "0.2.0", [] },
      { :decimal, "0.2.1", [] },
      { :ex_plex, "0.0.1", [] },
      { :ex_plex, "0.0.2", [decimal: "0.1.1"] },
      { :ex_plex, "0.1.0", [decimal: "~> 0.1.0"] },
      { :ex_plex, "0.1.2", [decimal: "~> 0.1.0"] },
      { :ex_plex, "0.2.0", [decimal: "~> 0.2.0"] },

      { :jose, "0.2.0", [] },
      { :jose, "0.2.1", [] },
      { :eric, "0.0.1", [] },
      { :eric, "0.0.2", [] },
      { :eric, "0.1.0", [jose: "~> 0.1.0"] },
      { :eric, "0.1.2", [jose: "~> 0.1.0"] },
      { :eric, "0.2.0", [jose: "~> 0.3.0"] },

      { :ex_doc, "0.0.1", [] },
      { :ex_doc, "0.0.2", [] },
      { :ex_doc, "0.1.0", [] },
      { :postgrex, "0.2.0", [ex_doc: "0.0.1"] },
      { :postgrex, "0.2.1", [ex_doc: "~> 0.1.0"] },
      { :ecto, "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"] },
      { :ecto, "0.2.1", [postgrex: "~> 0.2.0", ex_doc: "0.0.2"] } ]
  end

  using do
    quote do
      import unquote(__MODULE__)
    end
  end

  setup_all do
    File.mkdir_p!(tmp_path)
    ets_path = tmp_path("hex.ets")
    File.rm(ets_path)
    create_test_registry(ets_path)
    :ok
  end

  setup do
    Mix.shell(Mix.Shell.Process)
    Mix.Task.clear
    Mix.Shell.Process.flush
    Mix.ProjectStack.clear_cache
    Mix.ProjectStack.clear_stack
    :ok
  end

  teardown do
    Hex.stop
    :ok
  end
end

alias HexTest.Case


Case.init_fixture("ecto", "0.2.0", [
  { :postgrex, "~> 0.2.0", package: true },
  { :ex_doc, "~> 0.0.1", package: true }
])

Case.init_fixture("ecto", "0.2.1", [
  { :postgrex, "~> 0.2.0", package: true },
  { :ex_doc, "~> 0.0.2", package: true }
])

Case.init_fixture("ex_doc", "0.1.0", [])
Case.init_fixture("ex_doc", "0.0.1", [])

Case.init_fixture("postgrex", "0.2.1", [
  { :ex_doc, "~> 0.1.0", package: true }
])

Case.init_fixture("postgrex", "0.2.0", [
  { :ex_doc, "0.0.1", package: true }
])


if :integration in ExUnit.configuration[:include] do
  db = "hex_client_test"
  db_url = "ecto://postgres:postgres@localhost/#{db}"

  System.put_env("DATABASE_URL", db_url)
  :application.ensure_all_started(:hex_web)
  HexWeb.Config.password_work_factor(4)

  File.cd! "_build/test/lib/hex_web", fn ->
    Mix.Task.run "ecto.drop", ["HexWeb.Repo"]
    Mix.Task.run "ecto.create", ["HexWeb.Repo"]
    Mix.Task.run "ecto.migrate", ["HexWeb.Repo"]
  end

  Hex.start_api()
  Hex.url("http://localhost:4000")

  meta = [
    { "contributors", ["John Doe", "Jane Doe"] },
    { "licenses", ["GPL2", "MIT", "Apache"] },
    { "links", [{ "docs", "http://docs" }, { "repo", "http://repo" }] },
    { "description", "builds docs" } ]

  { :ok, user }    = HexWeb.User.create("user", "user@mail.com", "hunter42")
  { :ok, package } = HexWeb.Package.create("ex_doc", user, meta)
  { :ok, _ }       = HexWeb.Release.create(package, "0.0.1", Case.fixture_path("ex_doc-0.0.1"), "HEAD", [])
end

Mix.shell(Mix.Shell.Process)
Mix.Task.clear
Mix.Shell.Process.flush
Mix.ProjectStack.clear_cache
Mix.ProjectStack.clear_stack

Mix.SCM.append(Hex.SCM)
Mix.RemoteConverger.register(Hex.RemoteConverger)
