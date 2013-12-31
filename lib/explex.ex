defmodule Explex do
  def start(opts // []) do
    Explex.Registry.start(opts)
  end

  def stop do
    Explex.Registry.stop
  end
end
