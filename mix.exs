defmodule Hex.MixProject do
  use Mix.Project

  @version "0.20.6-dev"

  {:ok, system_version} = Version.parse(System.version())
  @elixir_version {system_version.major, system_version.minor, system_version.patch}

  def project do
    [
      app: :hex,
      version: @version,
      elixir: "~> 1.0",
      aliases: aliases(),
      lockfile: lockfile(@elixir_version),
      deps: deps(@elixir_version),
      elixirc_options: elixirc_options(Mix.env()),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  def application do
    [
      applications: applications(Mix.env()),
      mod: {Hex.Application, []}
    ]
  end

  defp applications(:prod), do: [:ssl, :inets]
  defp applications(_), do: [:ssl, :inets, :logger]

  # We use different versions of plug because older plug version produces
  # warnings on elixir >=1.3.0 and newer plug versions do not work on elixir <1.2.3
  defp lockfile(elixir_version) when elixir_version >= {1, 5, 0}, do: "mix-new.lock"
  defp lockfile(_), do: "mix-old.lock"

  # Can't use hex dependencies because the elixir compiler loads dependencies
  # and calls the dependency SCM. This would cause us to crash if the SCM was
  # Hex because we have to unload Hex before compiling it.
  defp deps(elixir_version) when elixir_version >= {1, 5, 0} do
    [
      {:stream_data, [github: "whatyouhide/stream_data", tag: "v0.4.0"] ++ test_opts()},
      {:plug, [github: "elixir-lang/plug", tag: "v1.6.1"] ++ test_opts()},
      {:mime, [github: "elixir-plug/mime", tag: "v1.3.0"] ++ test_opts()}
    ] ++ deps()
  end

  defp deps(_) do
    [
      {:plug, [github: "elixir-lang/plug", tag: "v1.1.6"] ++ test_opts()},
      {:mime, [github: "elixir-lang/mime", tag: "v1.0.1"] ++ test_opts()}
    ] ++ deps()
  end

  defp deps do
    [
      {:bypass, [github: "PSPDFKit-labs/bypass", only: :test]},
      {:cowboy, [github: "ninenines/cowboy", tag: "1.0.4", manager: :rebar3] ++ test_opts()},
      {:cowlib, [github: "ninenines/cowlib", tag: "1.0.2", manager: :rebar3] ++ test_opts()},
      {:ranch, [github: "ninenines/ranch", tag: "1.2.1", manager: :rebar3] ++ test_opts()}
    ]
  end

  defp test_opts(), do: [only: :test, override: true]

  defp elixirc_options(:prod), do: [debug_info: false]
  defp elixirc_options(_), do: []

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp aliases do
    [
      "compile.elixir": [&unload_hex/1, "compile.elixir"],
      run: [&unload_hex/1, "run"],
      install: ["archive.build -o hex.ez", "archive.install hex.ez --force"],
      certdata: [&certdata/1]
    ]
  end

  defp unload_hex(_) do
    Application.stop(:hex)
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
