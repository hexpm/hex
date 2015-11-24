defmodule HexTest.Case do
  use ExUnit.CaseTemplate

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
    Path.join(tmp_path, extension)
  end

  def fixture_path do
    Path.expand("../fixtures", __DIR__)
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
    meta = Map.put_new(meta, :build_tools, ["mix"])

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
      Enum.reduce(test_registry, %{}, fn {name, vsn, _}, dict ->
        Map.update(dict, "#{name}", [vsn], &[vsn|&1])
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
      {:phoenix, "0.0.1", [postgrex: "~> 0.2"]},

      {:only_doc, "0.1.0", [{:ex_doc, ">= 0.0.0", true}]},

      {:has_optional, "0.1.0", [{:ex_doc, "~> 0.0.1", true}]},

      {:package_name, "0.1.0", []},
      {:depend_name, "0.2.0", [{:package_name, ">= 0.0.0", false, :app_name}]} ]
  end

  def setup_auth(username) do
    user = HexWeb.User.get(username: username)
    key  = :crypto.rand_bytes(4) |> Base.encode16

    {:ok, key} = HexWeb.API.Key.create(user, %{"name" => key})
    Hex.Config.update([key: key.user_secret])
  end

  using do
    quote do
      import unquote(__MODULE__)
    end
  end

  setup_all do
    ets_path = tmp_path("registry.ets")
    File.rm(ets_path)
    create_test_registry(ets_path)
    :ok
  end

  @registry_tid :registry_tid

  setup do
    Hex.State.put(:home, tmp_path("hex_home"))
    Hex.State.put(:registry_updated, true)
    Hex.Parallel.clear(:hex_fetcher)
    Hex.Registry.close

    Mix.shell(Mix.Shell.Process)
    Mix.Task.clear
    Mix.Shell.Process.flush
    Mix.ProjectStack.clear_cache
    Mix.ProjectStack.clear_stack

    :ok
  end
end
