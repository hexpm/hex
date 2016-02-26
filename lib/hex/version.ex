defmodule Hex.Version do
  use GenServer
  alias Version.InvalidVersionError
  alias Version.InvalidRequirementError

  defmodule Requirement do
    defstruct [:source, :req]
  end

  @ets :hex_version

  def start do
    :ets.new(@ets, [:named_table, :public])
    {:ok, []}
  end

  def match?(version, requirement) do
    cache({:match?, version, requirement}, fn ->
      version     = parse!(version)
      requirement = parse_requirement!(requirement)
      custom_match?(version, requirement)
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

  def parse_requirement(%Requirement{} = requirement), do: {:ok, requirement}
  def parse_requirement(requirement) do
    cache({:req, requirement}, fn ->
      custom_requirement(requirement)
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

  defp custom_match?(version, %Requirement{req: req}),
    do: custom_match?(version, req)
  defp custom_match?(version, {"and", x, y}),
    do: custom_match?(version, x) and custom_match?(version, y)
  defp custom_match?(version, {"or", x, y}),
    do: custom_match?(version, x) or custom_match?(version, y)
  defp custom_match?(version, {%Version.Requirement{} = req, true}),
    do: Version.match?(version, req)
  defp custom_match?(%Version{pre: []} = version, {%Version.Requirement{} = req, false}),
    do: Version.match?(version, req)
  defp custom_match?(_version, _req),
    do: false

  defp custom_requirement(requirement) do
    try do
      req = :binary.split(requirement, " ", [:global, :trim_all]) |> custom_parse
      {:ok, %Requirement{source: requirement, req: req}}
    catch
      :error ->
        :error
    end
  end

  @version_ops ~w(~> == != <= >= < >)
  @bool_ops ~w(and or)

  defp custom_parse([op, version]) when op in @version_ops do
    pre? = :binary.match(version, "-") != :nomatch
    case Version.parse_requirement(op <> " " <> version) do
      {:ok, req} -> {req, pre?}
      :error     -> throw :error
    end
  end
  defp custom_parse([op1, version, op2 | rest]) when op2 in @bool_ops,
    do: {op2, custom_parse([op1, version]), custom_parse(rest)}
  defp custom_parse([version]),
    do: custom_parse(["==", version])
  defp custom_parse(_),
    do: throw :error
end
