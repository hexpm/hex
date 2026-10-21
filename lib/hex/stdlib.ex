defmodule Hex.Stdlib do
  @moduledoc false

  # Mix may remove the code paths of applications the project doesn't depend
  # on. Restore an application's code paths before calling into it.
  def ensure_application!(app) do
    if function_exported?(Mix, :ensure_application!, 1) do
      apply(Mix, :ensure_application!, [app])
    end
  end
end
