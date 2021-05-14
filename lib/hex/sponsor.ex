defmodule Hex.Sponsor do
  @metadata_file "hex_metadata.config"
  @sponsor_link_name "sponsor"

  def get_link(package_name, deps_path) do
    with {:ok, metadata} <- read_metadata(package_name, deps_path),
         {_, value} <- sponsorship(metadata) do
      value
    else
      _ -> nil
    end
  end

  defp read_metadata(package, deps_path) do
    :file.consult(Path.join([deps_path, package, @metadata_file]))
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
