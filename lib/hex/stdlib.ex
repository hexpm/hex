defmodule Hex.Stdlib do
  @moduledoc false

  # TODO: Remove this once we require OTP 24.0
  def ssh_hostkey_fingerprint(digset_type, key) do
    cond do
      # Requires Elixir 1.15.0
      function_exported?(Mix, :ensure_application!, 1) ->
        apply(Mix, :ensure_application!, [:ssh])
        apply(:ssh, :hostkey_fingerprint, [digset_type, key])

      Code.ensure_loaded?(:ssh) and function_exported?(:ssh, :hostkey_fingerprint, 2) ->
        apply(:ssh, :hostkey_fingerprint, [digset_type, key])

      true ->
        apply(:public_key, :ssh_hostkey_fingerprint, [digset_type, key])
    end
  end

  # Compilation prunes code paths for isolation, which may remove archive
  # paths like Hex. Restore them so all Hex modules are available.
  def ensure_application!(app) do
    if function_exported?(Mix, :ensure_application!, 1) do
      apply(Mix, :ensure_application!, [app])
    end
  end
end
