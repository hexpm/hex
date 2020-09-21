if Mix.env() == :dev do
  defmodule Hex.Dev do
    @moduledoc false

    @ets_name __MODULE__
    @registry_filename "cache.ets"
    @repo "hexpm"
    @ets_version 3

    def extract_registry(packages, new_path) do
      {:ok, original_ets} = :ets.file2tab(Hex.Stdlib.to_charlist(ets_path()))
      new_ets = :ets.new(@ets_name, [])

      try do
        :ets.insert(new_ets, {:version, @ets_version})
        copy(original_ets, new_ets, :last_update)

        Enum.each(packages, fn package ->
          copy_package(original_ets, new_ets, package)
        end)

        :ok = :ets.tab2file(new_ets, Hex.Stdlib.to_charlist(new_path))
      after
        :ets.delete(original_ets)
        :ets.delete(new_ets)
      end
    end

    defp copy_package(original_ets, new_ets, package) do
      unless :ets.member(new_ets, {:versions, @repo, package}) do
        copy(original_ets, new_ets, {:versions, @repo, package})
        copy(original_ets, new_ets, {:registry_etag, @repo, package})
        copy(original_ets, new_ets, {:timestamp, @repo, package})

        case :ets.lookup(original_ets, {:versions, @repo, package}) do
          [{_, versions}] ->
            IO.puts("COPYING #{package}")

            Enum.each(versions, fn version ->
              copy(original_ets, new_ets, {:deps, @repo, package, version})
              copy(original_ets, new_ets, {:inner_checksum, @repo, package, version})
              copy(original_ets, new_ets, {:outer_checksum, @repo, package, version})
              copy(original_ets, new_ets, {:retired, @repo, package, version})
              copy(original_ets, new_ets, {:timestamp, @repo, package, version})

              [{_, dep_tuples}] = :ets.lookup(original_ets, {:deps, @repo, package, version})

              Enum.each(dep_tuples, fn {@repo, dependency, _app, _requirement, _optional} ->
                copy_package(original_ets, new_ets, dependency)
              end)
            end)

          [] ->
            :ok
        end
      end
    end

    defp copy(original_ets, new_ets, key) do
      tuples = :ets.lookup(original_ets, key)
      :ets.insert(new_ets, tuples)
    end

    defp ets_path() do
      Path.join(Hex.State.fetch!(:cache_home), @registry_filename)
    end
  end
end
