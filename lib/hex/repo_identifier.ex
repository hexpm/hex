defmodule Hex.RepoIdentifier do
  @moduledoc """
  Gets an anonymized identifier for the current git repository.

  This module caches the SHA of the first commit in the repository and hashes it once more for
  anonymization.

  Returns `nil` when:

  - The `no_repo_identifier` config setting is set to `true`
  - The `HEX_NO_REPO_IDENTIFIER` environment variable is set to `1` or `true`
  - The `git` executable isn't available
  - The current directory isn't within a git repository
  """

  use Agent

  def start_link(_args) do
    Agent.start_link(fn -> :not_fetched end, name: __MODULE__)
  end

  def fetch do
    Agent.get_and_update(__MODULE__, fn
      :not_fetched ->
        value = get()

        {value, value}

      cached ->
        {cached, cached}
    end)
  end

  def put(value) do
    Agent.update(__MODULE__, fn _value -> value end)
  end

  def get do
    cond do
      Hex.State.get(:no_repo_identifier) ->
        nil

      output = initial_commit_sha() ->
        output
        |> String.trim()
        |> then(&:crypto.hash(:sha256, &1))
        |> Base.encode16(case: :lower)

      true ->
        nil
    end
  end

  def clear do
    Agent.update(__MODULE__, fn _value -> :not_fetched end)
  end

  defp initial_commit_sha do
    cmd_args = ~w(rev-list --max-parents=0 HEAD)

    with path when is_binary(path) <- System.find_executable("git") do
      case System.cmd("git", cmd_args, stderr_to_stdout: true) do
        {output, 0} ->
          output

        {output, exit_status} ->
          Hex.Shell.debug(
            "Unable to extract git identifier: (Exit #{exit_status})\n  #{String.trim(output)}"
          )

          nil
      end
    end
  end
end
