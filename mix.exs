defmodule Hex.MixProject do
  use Mix.Project

  @version "2.0.7-dev"

  def project do
    [
      app: :hex,
      version: @version,
      elixir: "~> 1.5",
      aliases: aliases(),
      preferred_cli_env: ["deps.get": :test],
      config_path: config_path(),
      deps: deps(Mix.env()),
      elixirc_options: elixirc_options(Mix.env()),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  def application do
    [
      extra_applications: [:ssl, :inets, :logger],
      mod: {Hex.Application, []}
    ]
  end

  defp deps(:test) do
    [
      {:bypass, "~> 1.0.0"},
      {:cowboy, "~> 2.7.0"},
      {:mime, "~> 1.0"},
      {:plug, "~> 1.9.0"},
      {:plug_cowboy, "~> 2.1.0"},
      {:plug_crypto, "~> 1.1.2"}
    ]
  end

  defp deps(_) do
    []
  end

  defp elixirc_options(:prod), do: [debug_info: false]
  defp elixirc_options(_), do: []

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp config_path() do
    if Version.compare(System.version(), "1.11.0") in [:eq, :gt] do
      "config/config.exs"
    else
      "config/mix_config.exs"
    end
  end

  defp aliases do
    [
      "compile.elixir": [&unload_hex/1, "compile.elixir"],
      run: [&unload_hex/1, "run"],
      install: ["archive.build -o hex.ez", "archive.install hex.ez --force"],
      certdata: [&certdata/1]
    ]
  end

  defp unload_hex(_) do
    update_cached_deps(__MODULE__)

    Application.stop(:hex)
    Application.unload(:hex)
    paths = Path.wildcard(Path.join(archives_path(), "hex*"))

    Enum.each(paths, fn archive ->
      ebin = archive_ebin(archive)
      Code.delete_path(ebin)

      {:ok, files} = ebin |> :unicode.characters_to_list() |> :erl_prim_loader.list_dir()

      Enum.each(files, fn file ->
        file = List.to_string(file)
        size = byte_size(file) - byte_size(".beam")

        case file do
          <<name::binary-size(size), ".beam">> ->
            module = String.to_atom(name)
            :code.delete(module)
            :code.purge(module)

          _ ->
            :ok
        end
      end)
    end)
  end

  defp update_cached_deps(module) do
    cond do
      Version.compare(System.version(), "1.7.0") == :lt ->
        if cached_deps = Mix.ProjectStack.read_cache({:cached_deps, Mix.env(), module}) do
          cached_deps = Enum.map(cached_deps, &change_scm/1)
          Mix.ProjectStack.write_cache({:cached_deps, Mix.env(), module}, cached_deps)
        end

      Version.compare(System.version(), "1.10.0") == :lt ->
        case Mix.ProjectStack.read_cache({:cached_deps, module}) do
          nil ->
            :ok

          {env_target, cached_deps} ->
            cached_deps = Enum.map(cached_deps, &change_scm/1)
            Mix.ProjectStack.write_cache({:cached_deps, module}, {env_target, cached_deps})
        end

      true ->
        case Mix.State.read_cache({:cached_deps, module}) do
          nil ->
            :ok

          {env_target, cached_deps} ->
            cached_deps = Enum.map(cached_deps, &change_scm/1)
            Mix.State.write_cache({:cached_deps, module}, {env_target, cached_deps})
        end
    end
  end

  defp change_scm(%Mix.Dep{deps: deps} = dep) do
    %Mix.Dep{dep | scm: Hex.FakeSCM, deps: Enum.map(deps, &change_scm/1)}
  end

  @mk_ca_bundle_url "https://raw.githubusercontent.com/bagder/curl/master/lib/mk-ca-bundle.pl"
  @mk_ca_bundle_cmd "mk-ca-bundle.pl"
  @ca_bundle "ca-bundle.crt"
  @ca_bundle_target Path.join("lib/hex/http", @ca_bundle)

  defp certdata(_) do
    cmd("wget", [@mk_ca_bundle_url])
    File.chmod!(@mk_ca_bundle_cmd, 0o755)

    cmd(Path.expand(@mk_ca_bundle_cmd), ["-u"])

    File.cp!(@ca_bundle, @ca_bundle_target)
    File.rm!(@ca_bundle)
    File.rm!(@mk_ca_bundle_cmd)
  end

  defp cmd(cmd, args) do
    {_, result} = System.cmd(cmd, args, into: IO.stream(:stdio, :line), stderr_to_stdout: true)

    if result != 0 do
      raise "Non-zero result (#{result}) from: #{cmd} #{Enum.map_join(args, " ", &inspect/1)}"
    end
  end

  cond do
    function_exported?(Mix, :path_for, 1) ->
      defp archives_path(), do: Mix.path_for(:archives)

    function_exported?(Mix.Local, :path_for, 1) ->
      defp archives_path(), do: Mix.Local.path_for(:archive)

    true ->
      defp archives_path(), do: Mix.Local.archives_path()
  end

  if function_exported?(Mix.Local, :archive_ebin, 1) do
    defp archive_ebin(archive), do: Mix.Local.archive_ebin(archive)
  else
    defp archive_ebin(archive), do: Mix.Archive.ebin(archive)
  end
end

defmodule Hex.FakeSCM do
  def fetchable?, do: true
end
