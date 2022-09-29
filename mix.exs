defmodule Hex.MixProject do
  use Mix.Project

  @version "2.0.0-dev"

  def project do
    [
      app: :hex,
      version: @version,
      elixir: "~> 1.5",
      aliases: aliases(),
      deps: deps(),
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

  defp deps() do
    [
      {:stream_data, [github: "whatyouhide/stream_data", tag: "v0.4.0"] ++ test_opts()},
      {:plug, [github: "elixir-lang/plug", tag: "v1.6.1"] ++ test_opts()},
      {:mime, [github: "elixir-plug/mime", tag: "v1.3.0"] ++ test_opts()},
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
