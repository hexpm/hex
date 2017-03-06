defmodule Mix.Tasks.Hex.PublicKeys do
  use Mix.Task

  @moduledoc false

  def run(_args) do
    Mix.raise "Deprecated in favor of mix hex.user key"
  end
end
