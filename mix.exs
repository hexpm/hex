defmodule Hex.Mixfile do
  use Mix.Project

  def project do
    [app: :hex,
     version: "0.4.2-dev",
     elixir: "~> 1.0.0-rc1",
     aliases: aliases,
     deps: deps]
  end

  def application do
    []
  end

  defp deps do
    [{:hex_web, github: "hexpm/hex_web", only: :test, env: :test}]
  end

  defp aliases do
    [compile: [&unload_hex/1, "compile"],
     run:     [&unload_hex/1, "run"]]
  end

  defp unload_hex(_) do
    paths = Path.join(Mix.Local.archives_path, "hex*.ez")
            |> Path.wildcard

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
end
