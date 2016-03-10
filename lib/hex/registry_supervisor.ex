defmodule Hex.RegistrySupervisor do
  use Supervisor

  @name __MODULE__

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: @name)
  end

  def init([]) do
    children = [
      worker(Hex.Registry, []),
      worker(Hex.PackageRegistry, []),
      supervisor(Hex.PackageRegistrySupervisor, []),
    ]

    supervise(children, strategy: :rest_for_one)
  end
end
