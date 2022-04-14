defmodule Hex.Netrc.Parser do
  @moduledoc false

  def parse(path \\ netrc_path()) when is_binary(path) do
    case File.read(path) do
      {:ok, contents} ->
        parse_contents(contents)

      error ->
        error
    end
  end

  defp parse_contents(contents) when is_binary(contents) do
    parse_result =
      contents
      |> String.trim()
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split/1)
      |> Enum.reduce({%{}, nil}, &parse_line/2)

    case parse_result do
      {machines, %{username: _, password: _} = current} ->
        {host, machine} = Map.pop(current, :host)
        {:ok, Map.put(machines, host, machine)}

      _ ->
        {:error, :parse}
    end
  end

  defp parse_line(_, :parse_error), do: :parse_error

  defp parse_line(["machine", host], {machines, nil}) do
    {machines, %{host: host}}
  end

  defp parse_line(["machine", next_host], {machines, %{username: _, password: _} = current}) do
    {host, machine} = Map.pop(current, :host)
    {Map.put(machines, host, machine), %{host: next_host}}
  end

  defp parse_line(["login", username], {machines, %{} = current}) do
    {machines, Map.put(current, :username, username)}
  end

  defp parse_line(["password", password], {machines, %{} = current}) do
    {machines, Map.put(current, :password, password)}
  end

  defp parse_line(_line, _parse_state), do: :parse_error

  def netrc_path() do
    System.get_env("NETRC") || default_path()
  end

  defp default_path() do
    Path.join(System.user_home!(), ".netrc")
  end
end
