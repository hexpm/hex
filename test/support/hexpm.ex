defmodule HexTest.Hexpm do
  import ExUnit.Assertions

  defmodule WrappedCollectable do
    defstruct [:collectable, :fun]

    defimpl Collectable do
      def into(%{collectable: collectable, fun: fun}) do
        {term, collectable_fun} = Collectable.into(collectable)
        {term, wrap(collectable_fun, fun)}
      end

      defp wrap(collectable_fun, fun) do
        fn
          term, {:cont, x} ->
            collectable_fun.(term, {:cont, fun.(x)})
          term, command ->
            collectable_fun.(term, command)
        end
      end
    end
  end

  defp stream_hexpm do
    %WrappedCollectable{
      collectable: IO.stream(:stdio, :line),
      fun: &IO.ANSI.format([:blue, &1])
    }
  end

  @mix_exs_template """
  defmodule ~s.NoConflict.MixProject do
    use Mix.Project

    def project do
      [app: :~s,
       version: "~s",
       deps: deps()]
    end

    defp deps do
      ~s
    end
  end
  """

  def init() do
    check_hexpm()
    mix = hexpm_mix() |> List.to_string()

    cmd(mix, ["ecto.drop", "-r", "Hexpm.Repo", "--quiet"])
    cmd(mix, ["ecto.create", "-r", "Hexpm.Repo", "--quiet"])
    cmd(mix, ["ecto.migrate", "-r", "Hexpm.Repo"])
  end

  def start() do
    path = Hex.string_to_charlist(path())
    hexpm_mix_home = Hex.string_to_charlist(hexpm_mix_home())
    hexpm_mix_archives = Hex.string_to_charlist(hexpm_mix_archives())

    key =
      Path.join(__DIR__, "../fixtures/test_priv.pem")
      |> File.read!()
      |> Hex.string_to_charlist()

    env = [
      {'MIX_ENV', 'hex'},
      {'MIX_HOME', hexpm_mix_home},
      {'MIX_ARCHIVES', hexpm_mix_archives},
      {'PATH', path},
      {'HEX_SIGNING_KEY', key}
    ]

    spawn_link(fn ->
      port = Port.open({:spawn_executable, hexpm_mix()}, [
                       :exit_status,
                       :use_stdio,
                       :stderr_to_stdout,
                       :binary,
                       :hide,
                       env: env,
                       cd: hexpm_dir(),
                       args: ["phx.server"]])

      fun = fn fun ->
        receive do
          {^port, {:data, data}} ->
            IO.ANSI.format([:blue, data]) |> IO.write
            fun.(fun)
          {^port, {:exit_status, status}} ->
            IO.puts "Hexpm quit with status #{status}"
            System.halt(status)
        end
      end

      fun.(fun)
    end)

    wait_on_start()
  end

  defp check_hexpm do
    dir = hexpm_dir()

    unless File.exists?(dir) do
      IO.puts "Unable to find #{dir}, make sure to clone the hexpm repository " <>
              "into it to run integration tests or set HEXPM_PATH to its location"
      System.halt(1)
    end
  end

  defp hexpm_dir do
    System.get_env("HEXPM_PATH") || "../hexpm"
  end

  defp hexpm_mix do
    if path = hexpm_elixir() do
      path = Hex.string_to_charlist(path)
      :os.find_executable('mix', path)
    else
      :os.find_executable('mix')
    end
  end

  defp hexpm_elixir do
    if path = System.get_env("HEXPM_ELIXIR_PATH") do
      path
      |> Path.expand
      |> Path.join("bin")
    end
  end

  defp hexpm_otp do
    if path = System.get_env("HEXPM_OTP_PATH") do
      path
      |> Path.expand
      |> Path.join("bin")
    end
  end

  defp hexpm_mix_home do
    (System.get_env("HEXPM_MIX_HOME") || Mix.Utils.mix_home)
    |> Path.expand
  end

  defp hexpm_mix_archives do
    archives_path =
      if function_exported?(Mix.Local, :path_for, 1),
        do: Mix.Local.path_for(:archive),
      else: Mix.Local.archives_path

    (System.get_env("HEXPM_MIX_ARCHIVES") || archives_path)
    |> Path.expand
  end

  defp cmd(command, args) do
    env = [
      {"MIX_ENV", "hex"},
      {"PATH", path()},
      {"MIX_HOME", hexpm_mix_home()},
      {"MIX_ARCHIVES", hexpm_mix_archives()},
    ]

    opts = [
        stderr_to_stdout: true,
        into: stream_hexpm(),
        env: env,
        cd: hexpm_dir()]

    0 = System.cmd(command, args, opts) |> elem(1)
  end

  defp path do
    [hexpm_elixir(), hexpm_otp(), System.get_env("PATH")]
    |> Enum.join(":")
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

  def new_repo(repository, auth) do
    Hex.API.erlang_post_request(nil, "repo", %{"name" => repository}, auth)
  end

  def new_user(username, email, password, key) do
    {:ok, {201, _, _}} = Hex.API.User.new(username, email, password)
    {:ok, {201, %{"secret" => secret}, _}} = Hex.API.Key.new(key, [user: username, pass: password])
    [key: secret, encrypted_key: Mix.Tasks.Hex.encrypt_key(password, secret)]
  end

  def new_key(auth) do
    {:ok, {201, %{"secret" => secret}, _}} = Hex.API.Key.new("key", auth)
    [key: secret]
  end

  def new_key(username, password, key) do
    {:ok, {201, %{"secret" => secret}, _}} = Hex.API.Key.new(key, [user: username, pass: password])
    [key: secret, encrypted_key: Mix.Tasks.Hex.encrypt_key(password, secret)]
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
        opts = Enum.into(opts, %{})
        default_opts = %{app: app, requirement: req, optional: false}
        {opts[:hex] || app, Map.merge(default_opts, opts)}
    end)

    meta =
      meta
      |> Map.merge(%{name: name, version: version, requirements: reqs})
      |> Map.put_new(:description, "empty")
      |> Map.put_new(:licenses, ["MIT"])
      |> Map.put_new(:app, name)
      |> Map.put_new(:build_tools, ["mix"])

    deps = inspect(deps, pretty: true)
    module = String.capitalize(name)
    mix_exs = :io_lib.format(@mix_exs_template, [module, name, version, deps])

    files = [{"mix.exs", List.to_string(mix_exs)}]
    {tar, _checksum} = Hex.create_tar!(meta, files, :memory)

    {:ok, {result, %{"version" => ^version}, _}} = Hex.API.Release.new("hexpm", name, tar, auth)
    assert result in [200, 201]
  end
end
