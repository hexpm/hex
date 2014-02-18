defmodule Explex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  def remote?({ app, _opts }) do
    remote?(app)
  end

  def remote?(app) when is_atom(app) do
    Explex.Registry.package_exists?(app)
  end

  def converge() do
    # TODO: Warn when requests are not in the registry

    lock = Mix.Deps.Lock.read
    locked = Explex.Mix.from_lock(lock)

    Mix.Project.config[:deps]
    |> Explex.Mix.deps_to_requirements
    |> Explex.Resolver.resolve(locked)
    |> Explex.Mix.to_lock(lock)
  end
end
