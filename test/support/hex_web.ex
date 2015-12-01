defmodule HexTest.HexWeb do
  import ExUnit.Assertions

  @mixfile_template """
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

  def init do
    cmd("mix", ["ecto.drop", "-r", "HexWeb.Repo", "--quiet"])
    cmd("mix", ["ecto.create", "-r", "HexWeb.Repo", "--quiet"])
    cmd("mix", ["ecto.migrate", "-r", "HexWeb.Repo"])
  end

  def start do
    mix = :os.find_executable('mix')
    port = Port.open({:spawn_executable, mix}, [
                     :exit_status,
                     :use_stdio,
                     :stderr_to_stdout,
                     :binary,
                     :hide,
                     env: [{'MIX_ENV', 'hex'}],
                     cd: hex_web_dir,
                     args: ["run", "--no-halt"]])

    fun = fn fun ->
      receive do
        {^port, {:data, data}} ->
          IO.write(data)
          fun.(fun)
        {^port, {:exit_status, status}} ->
          IO.puts "HexWeb quit with status #{status}"
          System.exit(status)
      end
    end

    spawn(fn -> fun.(fun) end)

    wait_on_start()
  end

  defp hex_web_dir do
    System.get_env("HEX_WEB_DIR") || "../hex_web"
  end

  defp cmd(command, args) do
    opts = [
        stderr_to_stdout: true,
        into: IO.stream(:stdio, :line),
        env: [{"MIX_ENV", "hex"}],
        cd: hex_web_dir]

    0 = System.cmd(command, args, opts) |> elem(1)
  end

  defp wait_on_start do
    case :httpc.request(:get, {'http://localhost:4043', []}, [], []) do
      {:ok, _} ->
        :ok
      {:error, _} ->
        :timer.sleep(10)
        wait_on_start()
    end
  end

  def new_user(username, email, password, key) do
    {201, _} = Hex.API.User.new(username, email, password)
    {201, %{"secret" => secret}} = Hex.API.Key.new(key, [user: username, pass: password])
    [key: secret]
  end

  def new_package(name, version, deps, meta, auth) do
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

    meta =
      meta
      |> Map.merge(%{name: name, version: version, requirements: reqs})
      |> Map.put_new(:description, "empty")
      |> Map.put_new(:app, name)
      |> Map.put_new(:build_tools, ["mix"])

    deps = inspect(deps, pretty: true)
    module = String.capitalize(name)
    mixfile = :io_lib.format(@mixfile_template, [module, name, version, deps])

    files = [{"mix.exs", List.to_string(mixfile)}]
    tar = Hex.Tar.create(meta, files)

    {result, %{"version" => ^version}} = Hex.API.Release.new(name, tar, auth)
    assert result in [200, 201]
  end
end
