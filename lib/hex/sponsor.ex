defmodule Hex.Sponsor do
  @metadata_file "hex_metadata.config"
  @sponsor_link_name "sponsor"

  def get_link(package_path) do
    with {:ok, metadata} <- read_metadata(package_path),
         {_, value} <- sponsorship(metadata) do
      value
    else
      _ -> nil
    end
  end

  def get_link(package_name, deps_path) do
    deps_path
    |> Path.join(package_name)
    |> get_link()
  end

  defp read_metadata(package_path) do
    package_path
    |> Path.join(@metadata_file)
    |> :file.consult()
  end

  defp sponsorship(metadata) do
    case Enum.find(metadata, fn {name, _} -> name == "links" end) do
      {_links, links} ->
        Enum.find(links, fn {link, _} -> String.downcase(link) == @sponsor_link_name end)

      _ ->
        nil
    end
  end
end
