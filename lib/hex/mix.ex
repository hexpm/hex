defmodule Hex.Mix do
  alias Hex.Registry

  def deps_to_requests(deps) do
    Enum.flat_map(deps, fn
      Mix.Dep[app: app, opts: opts, requirement: req] ->
        if opts[:package], do: [{ "#{app}", req }], else: []
      { app, opts } ->
        if opts[:package], do: [{ "#{app}", nil }], else: []
      { app, req, opts } ->
        if opts[:package], do: [{ "#{app}", req }], else: []
    end)
  end

  def from_lock(lock) do
    Enum.flat_map(lock, fn { name, opts } ->
      name = "#{name}"
      { url, ref } = from_lock_ref(opts)

      case Registry.version_from_ref(name, url, ref) do
        { :ok, version } -> [{ name, version }]
        :error -> []
      end
    end)
  end

  def annotate_deps(result, deps) do
    scms = Mix.SCM.available
    from = Path.absname("mix.exs")

    Enum.map(result, fn { app, version } ->
      atom = :"#{app}"
      { _, _, url, ref } = Registry.get_release(app, version)
      dep = Enum.find(deps, &(elem(&1, 0) == atom)) || { atom, package: true }

      Mix.Deps.Loader.to_dep(dep, scms, from)
        .update_deps(fn deps -> Enum.filter(deps, &(&1.scm == Hex.Mix.SCM)) end)
        .update_opts(&(&1 ++ [git_url: url, git_ref: ref]))
    end)
  end

  defp from_lock_ref({ :git, url, ref, _opts }), do: { url, ref }

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

    File.write!(path, Macro.to_string(updated_config))
  end

  defp config_path do
    Path.join(Mix.Utils.mix_home, "hex.config")
  end

  defp strip_block({ :__block__, _, [inner] }), do: inner
  defp strip_block(inner), do: inner
end
