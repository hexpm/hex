defmodule Hex.Version do
  @moduledoc false

  defmodule Requirement do
    @moduledoc false
    defstruct [:source, :req]
  end

  defmodule InvalidRequirementError do
    @moduledoc false
    defexception [:requirement]

    def exception(requirement) when is_binary(requirement) do
      %__MODULE__{requirement: requirement}
    end

    def message(%{requirement: requirement}) do
      "invalid requirement: #{inspect(requirement)}"
    end
  end

  defmodule InvalidVersionError do
    @moduledoc false
    defexception [:version]

    def exception(version) when is_binary(version) do
      %__MODULE__{version: version}
    end

    def message(%{version: version}) do
      "invalid version: #{inspect(version)}"
    end
  end

  @ets :hex_version

  def start do
    :ets.new(@ets, [:named_table, :public])
    {:ok, []}
  end

  def stable?(%Version{pre: []}), do: true
  def stable?(%Version{}), do: false
  def stable?(other), do: stable?(parse!(other))

  def match?(version, requirement, opts \\ []) do
    allow_pre = Keyword.get(opts, :allow_pre, false)
    req_source = requirement_source(requirement)

    cache({:match?, version, req_source, allow_pre}, fn ->
      version = parse!(version)
      requirement = parse_requirement!(req_source, allow_pre: allow_pre)

      cond do
        allow_pre_available?() ->
          Version.match?(version, requirement, allow_pre: allow_pre)

        allow_pre ->
          Version.match?(version, requirement)

        true ->
          custom_match?(version, requirement)
      end
    end)
  end

  def compare(version1, version2) do
    cache({:compare, version1, version2}, fn ->
      version1 = parse!(version1)
      version2 = parse!(version2)
      Version.compare(version1, version2)
    end)
  end

  def parse(%Version{} = version) do
    {:ok, version}
  end

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
        raise InvalidVersionError, version
    end
  end

  def parse_requirement(req, opts \\ [])

  def parse_requirement(%Requirement{} = req, _opts) do
    {:ok, req}
  end

  def parse_requirement(%Version.Requirement{} = req, _opts) do
    {:ok, req}
  end

  def parse_requirement(requirement, opts) do
    allow_pre = Keyword.get(opts, :allow_pre, false)

    cache({:req, requirement, allow_pre}, fn ->
      if allow_pre or allow_pre_available?() do
        case Version.parse_requirement(requirement) do
          {:ok, req} -> {:ok, compile_requirement(req)}
          :error -> :error
        end
      else
        custom_requirement(requirement)
      end
    end)
  end

  defp compile_requirement(req) do
    if allow_pre_available?() do
      Version.compile_requirement(req)
    else
      req
    end
  end

  def parse_requirement!(requirement, opts \\ []) do
    case parse_requirement(requirement, opts) do
      {:ok, requirement} ->
        requirement

      :error ->
        raise InvalidRequirementError, requirement
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

  defp requirement_source(%Requirement{source: source}), do: source
  defp requirement_source(%Version.Requirement{source: source}), do: source
  defp requirement_source(source), do: source

  defp custom_match?(version, %Requirement{req: req}) do
    custom_match?(version, req)
  end

  defp custom_match?(version, {"and", x, y}) do
    custom_match?(version, x) and custom_match?(version, y)
  end

  defp custom_match?(version, {"or", x, y}) do
    custom_match?(version, x) or custom_match?(version, y)
  end

  defp custom_match?(version, {%Version.Requirement{} = req, true}) do
    Version.match?(version, req)
  end

  defp custom_match?(%Version{pre: []} = version, {%Version.Requirement{} = req, false}) do
    Version.match?(version, req)
  end

  defp custom_match?(_version, _req) do
    false
  end

  defp custom_requirement(requirement) do
    try do
      req =
        requirement
        |> String.split(" ", trim: true)
        |> split_ops()
        |> custom_parse()

      {:ok, %Requirement{source: requirement, req: req}}
    catch
      :error ->
        :error
    end
  end

  @version_ops ~w(~> == != <= >= < >)
  @bool_ops ~w(and or)

  defp custom_parse([op, version]) when op in @version_ops do
    pre? = String.contains?(version, "-")

    case Version.parse_requirement(op <> " " <> version) do
      {:ok, req} ->
        {req, pre?}

      :error ->
        throw(:error)
    end
  end

  defp custom_parse([op1, version, op2 | rest]) when op2 in @bool_ops do
    {op2, custom_parse([op1, version]), custom_parse(rest)}
  end

  defp custom_parse([version]) do
    custom_parse(["==", version])
  end

  defp custom_parse(_) do
    throw(:error)
  end

  def split_ops([op | rest]) when op in @version_ops do
    [op | split_ops(rest)]
  end

  def split_ops([<<op::binary-2, version::binary>> | rest]) when op in @version_ops do
    [op, version | split_ops(rest)]
  end

  def split_ops([<<op::binary-1, version::binary>> | rest]) when op in @version_ops do
    [op, version | split_ops(rest)]
  end

  def split_ops([version | rest]) do
    [version | split_ops(rest)]
  end

  def split_ops([]) do
    []
  end

  defp allow_pre_available? do
    Code.ensure_loaded?(Version) and function_exported?(Version, :match?, 3)
  end

  def major_version_change?(%Version{major: major1}, %Version{major: major2})
      when major1 != major2,
      do: true

  def major_version_change?(_version1, _version2), do: false

  def breaking_minor_version_change?(%Version{major: 0, minor: minor1}, %Version{
        major: 0,
        minor: minor2
      })
      when minor1 != minor2,
      do: true

  def breaking_minor_version_change?(_version1, _version2), do: false
end
