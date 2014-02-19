defmodule Explex.SCM do
  @moduledoc false

  @behaviour Mix.SCM

  alias Mix.SCM.Git

  defdelegate [
    format_lock(opts),
    checked_out?(opts)
    ], to: Git

  def fetchable? do
    true
  end

  def format(_opts) do
    "package"
  end

  def accepts_options(_app, opts) do
    if opts[:package] do
      opts
    end
  end

  def lock_status(opts) do
    case opts[:lock] do
      { :git, lock_repo, lock_rev, _lock_opts } ->
        File.cd!(opts[:dest], fn ->
          rev_info = get_rev_info
          cond do
            lock_rev  != rev_info[:rev]    -> :mismatch
            lock_repo != rev_info[:origin] -> :outdated
            true                           -> :ok
          end
        end)
      nil ->
        :mismatch
      _ ->
        :outdated
    end
  end

  def equal?(_, _) do
    true
  end

  def checkout(opts) do
    # IO.inspect git_opts(opts)
    git_opts(opts) |> Git.checkout
  end

  def update(opts) do
    # IO.inspect git_opts(opts)
    git_opts(opts) |> Git.update
  end

  defp get_rev_info do
    destructure [origin, rev],
      System.cmd("git config remote.origin.url && git rev-parse --verify --quiet HEAD")
      |> String.split("\n", trim: true)
    [ origin: origin, rev: rev ]
  end

  defp git_opts(opts) do
    [ git: opts[:git_url], ref: opts[:git_ref] ] ++ opts
  end
end
