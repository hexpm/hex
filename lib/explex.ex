defmodule Explex do
  def start() do
    Explex.Registry.start()
    Mix.SCM.append(Explex.SCM)
    Mix.RemoteConverger.register(Explex.RemoteConverger)
  end

  def stop do
    Explex.Registry.stop
  end
end
