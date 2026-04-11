defmodule Hex.Stdlib do
  @moduledoc false

  # Compilation prunes code paths for isolation, which may remove archive
  # paths like Hex. Restore them so all Hex modules are available.
  def ensure_application!(app) do
    if function_exported?(Mix, :ensure_application!, 1) do
      apply(Mix, :ensure_application!, [app])
    end
  end
end
