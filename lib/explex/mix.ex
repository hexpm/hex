defmodule Explex.Mix do
  alias Explex.Registry
  alias Explex.Registry.Package

  def deps_to_requests(deps) do
    Enum.flat_map(deps, fn
      { name, opts } ->
        if opts[:package], do: [{ name, nil }], else: []
      { name, req, opts } ->
        if opts[:package], do: [{ name, req }], else: []
    end)
  end

  def from_lock(lock) do
    Enum.flat_map(lock, fn { name, opts } ->
      { url, ref } = from_lock_ref(opts)

      case Registry.version_from_ref(name, url, ref) do
        { :ok, version } -> [{ name, version }]
        :error -> []
      end
    end)
  end

  def to_lock(result, old_lock // []) do
    new_lock =
      Enum.map(result, fn { name, version } ->
        Package[url: url, ref: ref] = Registry.get_package(name, version)
        { name, to_lock_ref({ url, ref }) }
      end)
    Dict.merge(new_lock, old_lock)
  end

  defp from_lock_ref({ :git, url, ref, _opts }), do: { url, ref }

  defp to_lock_ref({ url, ref }), do: { :git, url, ref, [] }
end
