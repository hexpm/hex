defmodule Hex.Ignores do
  @moduledoc false

  # Parsing and matching for the ignore_advisories and ignore_retirements
  # configs. Parsed advisory entries are advisory ID strings. Parsed
  # retirement entries are {name, version} tuples where a nil version
  # matches every version of the package.

  def parse_advisories(nil), do: {:ok, []}
  def parse_advisories(""), do: {:ok, []}

  def parse_advisories(value) when is_binary(value) do
    {:ok, split_env_list(value)}
  end

  def parse_advisories(value) when is_list(value) do
    if Enum.all?(value, &(is_binary(&1) and &1 != "")) do
      {:ok, value}
    else
      :error
    end
  end

  def parse_advisories(_), do: :error

  def parse_retirements(nil), do: {:ok, []}
  def parse_retirements(""), do: {:ok, []}

  def parse_retirements(value) when is_binary(value) do
    value
    |> split_env_list()
    |> Enum.map(&parse_retirement_string/1)
    |> collect_entries()
  end

  def parse_retirements(value) when is_list(value) do
    value
    |> Enum.map(&parse_retirement_term/1)
    |> collect_entries()
  end

  def parse_retirements(_), do: :error

  def advisory_ignored?(advisory, ids) do
    Enum.any?(ids, &advisory_matches?(advisory, &1))
  end

  def advisory_matches?(%{id: advisory_id} = advisory, id) do
    id = String.downcase(id)

    Enum.any?([advisory_id | Map.get(advisory, :aliases, [])], fn candidate ->
      String.downcase(alias_id(candidate)) == id
    end)
  end

  def retirement_ignored?(package, version, entries) do
    Enum.any?(entries, &retirement_matches?(package, version, &1))
  end

  def retirement_matches?(package, _version, {name, nil}), do: package == name

  def retirement_matches?(package, version, {name, pinned}),
    do: package == name and version == pinned

  def split_advisories(advisories, []), do: {[], advisories}

  def split_advisories(advisories, ids) do
    ignored_ids =
      advisories
      |> :mix_hex_advisory.group_for_display()
      |> Enum.filter(&advisory_ignored?(&1, ids))
      |> Enum.flat_map(&group_identifiers/1)
      |> MapSet.new()

    Enum.split_with(advisories, fn %{id: id} ->
      MapSet.member?(ignored_ids, String.downcase(id))
    end)
  end

  def reject_ignored_advisories(advisories, ids) do
    {_ignored, active} = split_advisories(advisories, ids)
    active
  end

  defp split_env_list(value) do
    value
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp parse_retirement_string(entry) do
    case String.split(entry, "@", parts: 2) do
      [name] -> {:ok, {name, nil}}
      [name, version] -> entry(name, version)
    end
  end

  defp parse_retirement_term(name) when is_atom(name) and name not in [nil, true, false] do
    {:ok, {Atom.to_string(name), nil}}
  end

  defp parse_retirement_term({name, version})
       when is_atom(name) and name not in [nil, true, false] do
    entry(Atom.to_string(name), version)
  end

  defp parse_retirement_term(_), do: :error

  defp entry(name, version) when name != "" and is_binary(version) do
    case Version.parse(version) do
      {:ok, _} -> {:ok, {name, version}}
      :error -> :error
    end
  end

  defp entry(_name, _version), do: :error

  defp collect_entries(results) do
    if Enum.all?(results, &match?({:ok, _}, &1)) do
      {:ok, Enum.map(results, fn {:ok, entry} -> entry end)}
    else
      :error
    end
  end

  defp group_identifiers(%{id: id} = group) do
    aliases = Map.get(group, :aliases, [])
    [String.downcase(id) | Enum.map(aliases, &String.downcase(alias_id(&1)))]
  end

  defp alias_id(%{id: id}), do: id
  defp alias_id(id) when is_binary(id), do: id
end
