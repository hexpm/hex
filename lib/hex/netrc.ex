defmodule Hex.Netrc do
  @moduledoc false

  alias Hex.Netrc.Cache
  alias Hex.Netrc.Parser

  def lookup(host, path \\ Parser.netrc_path()) when is_binary(host) and is_binary(path) do
    case Cache.fetch(path) do
      {:ok, %{} = machines} ->
        {:ok, Map.get(machines, host)}

      other ->
        other
    end
  end
end
