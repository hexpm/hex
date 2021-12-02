defmodule Hex.Netrc.Cache do
  @moduledoc false

  alias Hex.Netrc.Parser

  @agent_name __MODULE__

  def start_link(_arg) do
    Agent.start_link(fn -> %{} end, name: @agent_name)
  end

  def child_spec(arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [arg]}
    }
  end

  def fetch(path \\ Parser.netrc_path()) when is_binary(path) do
    Agent.get_and_update(@agent_name, fn cache ->
      case Map.fetch(cache, path) do
        {:ok, cached_parse_result} ->
          {cached_parse_result, cache}

        :error ->
          parse_result = Parser.parse(path)
          {parse_result, Map.put(cache, path, parse_result)}
      end
    end)
  end
end
