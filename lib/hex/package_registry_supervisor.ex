defmodule Hex.PackageRegistrySupervisor do
  use Supervisor

  @name __MODULE__

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: @name)
  end

  def start_registry(module) do
    Supervisor.start_child(@name, worker(module, []))
  end

  def init([]) do
    children = [
    ]

    supervise(children, strategy: :one_for_one)
  end
end
