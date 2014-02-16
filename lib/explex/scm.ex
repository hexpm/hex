defmodule Explex.SCM do
  @moduledoc false

  @behaviour Mix.SCM

  alias Mix.SCM.Git

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
            lock_rev  != rev_info[:rev]      -> :mismatch
            lock_repo != rev_info[:origin]   -> :outdated
            true -> :ok
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

  defp get_rev_info do
    destructure [origin, rev],
      System.cmd('git config remote.origin.url && git rev-parse --verify --quiet HEAD')
      |> iolist_to_binary
      |> String.split("\n", trim: true)
    [ origin: origin, rev: rev ]
  end

  defdelegate [
    format_lock(opts),
    checked_out?(opts),
    checkout(opts),
    update(opts),
    ], to: Git
end
