defmodule Hex.Config do
  def read do
    Hex.Util.move_home

    case File.read(config_path) do
      {:ok, binary} ->
        case decode_term(binary) do
          {:ok, term} -> term ++ [source: :term]
          {:error, _} -> decode_elixir(binary) ++ [source: :elixir]
        end
      {:error, _} ->
        []
    end
  end

  def update(config) do
    read()
    |> Keyword.merge(config)
    |> write()
  end

  def write(config) do
    Hex.Util.move_home
    path = config_path

    File.mkdir_p!(Path.dirname(path))
    source = config[:source]
    config = Dict.delete(config, :source)

    if source == :term do
      string = encode_term(config)
    else
      string = encode_elixir(config)
    end

    File.write!(path, string)
  end

  defp config_path do
    Path.join(Hex.home, "hex.config")
  end

  defp encode_elixir(string) do
    Macro.to_string(string) <> "\n"
  end

  defp decode_elixir(string) do
    {term, _binding} = Code.eval_string(string)
    term
  end

  defp encode_term(list) do
    list
    |> Enum.map(&[:io_lib.print(&1) | ".\n\n"])
    |> IO.iodata_to_binary
  end

  defp decode_term(string) do
    {:ok, pid} = StringIO.open(string)
    try do
      consult(pid, [])
    after
      StringIO.close(pid)
    end
  end

  defp consult(pid, acc) when is_pid(pid) do
    case :io.read(pid, '') do
      {:ok, term}      -> consult(pid, [term|acc])
      {:error, reason} -> {:error, reason}
      :eof             -> {:ok, Enum.reverse(acc)}
    end
  end
end
