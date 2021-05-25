defmodule Hex.Sponsor do
  @moduledoc false

  @metadata_file "hex_metadata.config"
  @sponsor_link_name "sponsor"

  def get_link(package_path) do
    case read_metadata(package_path) do
      {:ok, metadata} -> sponsorship(metadata)
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
        Enum.find_value(links, fn {link, value} ->
          if String.downcase(link) == @sponsor_link_name do
            value
          end
        end)

      _ ->
        nil
    end
  end
end
