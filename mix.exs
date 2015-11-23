defmodule Hex.Mixfile do
  use Mix.Project

  def project do
    [app: :hex,
     version: "0.9.1-dev",
     aliases: aliases,
     deps: deps,
     elixirc_options: elixirc_options(Mix.env)]
  end

  def application do
    [applications: [:ssl, :inets],
     mod: {Hex, []}]
  end

  defp deps do
    [{:hex_web, github: "hexpm/hex_web", only: :test, env: :test}]
  end

  defp elixirc_options(:prod) do
    [debug_info: false]
  end

  defp elixirc_options(_) do
    []
  end

  defp aliases do
    [compile: ["deps.check", &unload_hex/1, "compile"],
     run: [&unload_hex/1, "run"],
     install: ["archive.build -o hex.ez", "archive.install hex.ez --force"],
     certdata: [&certdata/1]]
  end

  defp unload_hex(_) do
    paths = Path.join(Mix.Local.archives_path, "hex*.ez") |> Path.wildcard

    Enum.each(paths, fn archive ->
      ebin = Mix.Archive.ebin(archive)
      Code.delete_path(ebin)

      {:ok, files} = :erl_prim_loader.list_dir(to_char_list(ebin))

      Enum.each(files, fn file ->
        file = List.to_string(file)
        size = byte_size(file) - byte_size(".beam")

        case file do
          <<name :: binary-size(size), ".beam">> ->
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
  @ca_bundle        "ca-bundle.crt"
  @ca_bundle_target Path.join("lib/hex/api", @ca_bundle)

  defp certdata(_) do
    cmd("wget", [@mk_ca_bundle_url])
    File.chmod!(@mk_ca_bundle_cmd, 0o755)

    cmd(Path.expand(@mk_ca_bundle_cmd), ["-u"])

    File.cp!(@ca_bundle, @ca_bundle_target)
    File.rm!(@ca_bundle)
    File.rm!(@mk_ca_bundle_cmd)
  end

  defp cmd(cmd, args) do
    {_, result} = System.cmd(cmd, args, into: IO.stream(:stdio, :line),
                             stderr_to_stdout: true)

    if result != 0 do
      raise "Non-zero result (#{result}) from: #{cmd} #{Enum.map_join(args, " ", &inspect/1)}"
    end
  end
end
