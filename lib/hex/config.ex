defmodule Hex.Config do
  def read do
    Hex.Util.move_home

    case File.read(config_path) do
      {:ok, binary} ->
        {config, _binding} = Code.eval_string(binary)
        config
      {:error, _} ->
        []
    end
  end

  def update(config) do
    Hex.Util.move_home

    path = config_path
    updated_config =
      case File.read(path) do
        {:ok, binary} ->
          quoted = Code.string_to_quoted!(binary, file: path)
          Keyword.merge(quoted, config)
        {:error, _} ->
          config
      end

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Macro.to_string(updated_config) <> "\n")
  end

  def write(config) do
    Hex.Util.move_home
    path = config_path

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Macro.to_string(config) <> "\n")
  end

  defp config_path do
    Path.join(Hex.home, "hex.config")
  end
end
