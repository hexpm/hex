ExUnit.start exclude: [:integration]

defmodule ExplexTest.Case do
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
    flag = tmp_path |> String.to_char_list!

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
      System.cmd("git commit -m \"ok\"")
    end
  end

  def purge(modules) do
    Enum.each modules, fn(m) ->
      :code.delete(m)
      :code.purge(m)
    end
  end

  @dets_table :explex_dets_registry
  @version    1

  def create_test_registry(path) do
    dets_opts = [
      file: path,
      ram_file: true,
      type: :duplicate_bag ]

    packages =
      Enum.map(test_registry, fn { name, version, deps } ->
        url = fixture_path("#{name}-#{version}")
        { name, version, deps, url, "HEAD" }
      end)

    { :ok, @dets_table } = :dets.open_file(@dets_table, dets_opts)
    :ok = :dets.insert(@dets_table, { :"$$version$$", @version })
    :ok = :dets.insert(@dets_table, packages)
    :ok = :dets.close(@dets_table)
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

  def reset_db(repo, dir) do
    files = Path.join(dir, "*") |> Path.wildcard

    modules =
      Enum.map(files, fn file ->
        filename = Path.basename(file)
        { version, _ } = Integer.parse(filename)
        [{ module, _ }] = Code.load_file(file)
        { version, module }
      end) |> Enum.sort

    Enum.each(Enum.reverse(modules), fn { version, module } ->
      Ecto.Migrator.down(repo, version, module)
    end)

    Enum.each(modules, fn { version, module } ->
      :ok = Ecto.Migrator.up(repo, version, module)
    end)
  end

  using do
    quote do
      import unquote(__MODULE__)
    end
  end

  setup_all do
    dets_path = tmp_path("explex.dets")
    File.mkdir_p!(tmp_path)
    File.rm!(dets_path)
    create_test_registry(dets_path)
    :ok
  end

  setup do
    Mix.shell(Mix.Shell.Process)
    Mix.Task.clear
    Mix.Shell.Process.flush
    Mix.ProjectStack.clear_cache
    Mix.ProjectStack.clear_stack

    Explex.Registry.start [
      registry_path: tmp_path("explex.dets"),
      ram_file: true ]
    :ok
  end

  teardown do
    Explex.Registry.stop
    :ok
  end
end

alias ExplexTest.Case


Mix.shell(Mix.Shell.Process)
Mix.Task.clear
Mix.Shell.Process.flush
Mix.ProjectStack.clear_cache
Mix.ProjectStack.clear_stack

Mix.SCM.append(Explex.SCM)
Mix.RemoteConverger.register(Explex.RemoteConverger)

Case.init_fixture("ecto", "0.2.0", [
  { :git_repo, git: Case.fixture_path("git_repo-0.1.0") },
  { :postgrex, "~> 0.2.0", package: true },
  { :ex_doc, "~> 0.0.1", package: true }
])

Case.init_fixture("ex_doc", "0.0.1", [])

Case.init_fixture("git_repo", "0.1.0", [
  { :ex_doc, "~> 0.0.1", package: true }
])

Case.init_fixture("postgrex", "0.2.0", [
  { :ex_doc, "0.0.1", package: true }
])


if :integration in ExUnit.configuration[:include] do
  db = "explex_client_test"
  db_url = "ecto://explex:explex@localhost/#{db}"

  System.put_env("EXPLEX_ECTO_URL", db_url)
  { :ok, _ } = ExplexWeb.Repo.start_link
  Case.reset_db(ExplexWeb.Repo, "deps/explex_web/priv/migrations")
  ExplexWeb.Repo.stop

  Explex.start_api()
  Explex.url("http://localhost:4000")

  :application.ensure_all_started(:explex_web)
  :application.set_env(:explex_web, :password_work_factor, 4)

  { :ok, user }    = ExplexWeb.User.create("user", "user@mail.com", "hunter42")
  { :ok, package } = ExplexWeb.Package.create("ex_doc", user, [])
  { :ok, _ }       = ExplexWeb.Release.create(package, "0.0.1", Case.fixture_path("ex_doc-0.0.1"), "HEAD", [])
end
