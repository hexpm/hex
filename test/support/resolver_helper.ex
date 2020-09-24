defmodule HexTest.ResolverHelper do
  alias Hex.Registry.Server, as: Registry

  def resolve(reqs, locked \\ [], repos \\ %{}) do
    deps = deps(reqs)
    top_level = Enum.map(deps, &elem(&1, 0))
    reqs = reqs(reqs)
    locked = locked(locked)

    [reqs, locked]
    |> Enum.concat()
    |> Enum.map(&{elem(&1, 0), elem(&1, 1)})
    |> Registry.prefetch()

    case Hex.Resolver.resolve(Registry, reqs, deps, top_level, repos, locked) do
      {:ok, dict} -> dict
      {:error, {_reason, messages}} -> messages <> "\n"
    end
  end

  defp config({app, req}), do: {"hexpm", Atom.to_string(app), req}
  defp config({repo, app, req}), do: {Atom.to_string(repo), Atom.to_string(app), req}

  def deps(reqs) do
    Enum.into(reqs, %{}, fn dep ->
      {_repo, name, _req} = config(dep)
      {name, {false, %{}}}
    end)
  end

  defp reqs(reqs) do
    Enum.map(reqs, fn dep ->
      {repo, name, req} = config(dep)
      {repo, name, name, req, "mix.exs"}
    end)
  end

  def locked(locked) do
    Enum.map(locked, fn dep ->
      {repo, name, req} = config(dep)
      {repo, name, name, req}
    end)
  end

  def equal?(locked, resolved) do
    Enum.sort(locked) == Enum.sort(resolved)
  end
end
