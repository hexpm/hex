defmodule Hex.Mix do
  def deps_to_requests(deps) do
    # Ignore overriding of Hex deps for now
    overriden =
      for %Mix.Dep{app: app, scm: scm, opts: opts} <- deps,
        scm != Hex.SCM and opts[:override],
        do: app

    hex_deps =
      Enum.flat_map(deps, fn %Mix.Dep{deps: children} = dep ->
        hex_deps = Enum.filter(children, &hex_dep?/1)
        if hex_dep?(dep) do
          hex_deps = [dep|hex_deps]
        end
        hex_deps
      end)

    hex_deps = Enum.reject(hex_deps, fn %Mix.Dep{app: app} -> app in overriden end)

    for %Mix.Dep{app: app, requirement: req} <- hex_deps,
        not app in overriden,
        do: {"#{app}", req}
  end

  defp hex_dep?(%Mix.Dep{scm: Hex.SCM}), do: true
  defp hex_dep?(%Mix.Dep{}), do: false

  def dep({ app, opts }) when is_list(opts),
    do: { app, nil, opts }
  def dep({ app, req }) when is_binary(req),
    do: { app, req, [] }
  def dep({ app, req, opts }),
    do: { app, req, opts }

  def from_lock(lock) do
    for { name, { :package, version } } <- lock,
        into: %{},
        do: { "#{name}", version }
  end

  def to_lock(result) do
    for { name, version } <- result,
        into: %{},
        do: { :"#{name}", { :package, version } }
  end

  def read_config do
    case File.read(config_path) do
      { :ok, binary } ->
        { config, _binding } = Code.eval_string(binary)
        config
      { :error, _ } ->
        []
    end
  end

  def update_config(config) do
    path = config_path
    updated_config =
      case File.read(path) do
        { :ok, binary } ->
          quoted = Code.string_to_quoted!(binary, file: path) |> strip_block
          Keyword.merge(quoted, config)
        { :error, _ } ->
          config
      end

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Macro.to_string(updated_config) <> "\n")
  end

  defp config_path do
    Path.join(Mix.Utils.mix_home, "hex.config")
  end

  defp strip_block({ :__block__, _, [inner] }), do: inner
  defp strip_block(inner), do: inner
end
