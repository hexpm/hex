defmodule Hex.Version do
  use GenServer
  alias Version.InvalidVersionError
  alias Version.InvalidRequirementError

  @ets :hex_version

  def start do
    :ets.new(@ets, [:named_table, :public])
    {:ok, []}
  end

  def match?(version, requirement) do
    cache({:match?, version, requirement}, fn ->
      version     = parse!(version)
      requirement = parse_requirement!(requirement)
      Version.match?(version, requirement)
    end)
  end

  def compare(version1, version2) do
    cache({:compare, version1, version2}, fn ->
      version1 = parse!(version1)
      version2 = parse!(version2)
      Version.compare(version1, version2)
    end)
  end

  def parse(%Version{} = version), do: {:ok, version}
  def parse(version) do
    cache({:version, version}, fn ->
      Version.parse(version)
    end)
  end

  def parse!(version) do
    case parse(version) do
      {:ok, version} ->
        version
      :error ->
        raise InvalidVersionError, message: version
    end
  end

  def parse_requirement(%Version.Requirement{} = requirement), do: {:ok, requirement}
  def parse_requirement(requirement) do
    cache({:req, requirement}, fn ->
      Version.parse_requirement(requirement)
    end)
  end

  def parse_requirement!(requirement) do
    case parse_requirement(requirement) do
      {:ok, requirement} ->
        requirement
      :error ->
        raise InvalidRequirementError, message: requirement
    end
  end

  defp cache(key, fun) do
    case :ets.lookup(@ets, key) do
      [{_, value}] ->
        value
      [] ->
        value = fun.()
        :ets.insert(@ets, {key, value})
        value
    end
  end
end
