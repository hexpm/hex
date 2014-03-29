defmodule Hex.RemoteConverger do
  @moduledoc false

  @behaviour Mix.RemoteConverger

  @registry_updated :registry_updated

  def remote?(dep) do
    !! dep.opts[:hex_app]
  end

  def converge(deps) do
    unless File.exists?(Hex.Registry.path()) do
      if update_registry("Fetching registry...") == :error do
        raise Mix.Error
      end
    end

    Hex.Registry.start

    main      = Mix.project[:deps] || []
    lock      = Mix.Dep.Lock.read
    locked    = Hex.Mix.from_lock(lock)
    reqs      = Hex.Mix.deps_to_requests(deps)
    overriden = Hex.Mix.overriden(main)

    print_info(reqs, locked)

    if resolved = Hex.Resolver.resolve(reqs, overriden, locked) do
      print_success(resolved, locked)
      Hex.Mix.annotate_deps(resolved, deps)
    else
      raise Mix.Error, message: "Dependency resolution failed, relax the version requirements or unlock dependencies"
    end
  end

  def update_registry(info \\ nil) do
    if :application.get_env(:hex, :registry_updated) == { :ok, true } do
      { :ok, :cached }
    else
      :application.set_env(:hex, :registry_updated, true)

      if info, do: Mix.shell.info(info)

      path = Hex.Registry.path
      path_gz = Hex.Registry.path <> ".gz"

      case File.read(path_gz) do
        { :ok, contents } ->
          etag = :crypto.hash(:md5, contents) |> Hex.Util.hexify
          opts = [etag: etag]
        { :error, _ } ->
          opts = []
      end

      case Hex.API.get_registry(opts) do
        { 200, body } ->
          File.write!(path_gz, body)
          data = :zlib.gunzip(body)
          File.write!(path, data)
          Mix.shell.info("Registry update was successful!")
          { :ok, :new }
        { 304, _ } ->
          Mix.shell.info("Registry was fresh!")
          { :ok, :new }
        { code, body } ->
          Mix.shell.error("Registry update failed! (#{code})")
          Mix.Tasks.Hex.Util.print_error_result(code, body)
          :error
      end
    end
  end

  defp print_info(reqs, locked) do
    resolve =
      Enum.flat_map(reqs, fn { app, _req} ->
        if Dict.has_key?(locked, app), do: [], else: [app]
      end)

    if resolve != [] do
      Mix.shell.info "Running dependency resolution for unlocked dependencies: " <> Enum.join(resolve, ", ")
    end
  end

  defp print_success(resolved, locked) do
    resolved = Dict.drop(resolved, Dict.keys(locked))
    if resolved != [] do
      Mix.shell.info "Dependency resolution completed successfully"
      Enum.each(resolved, fn { dep, version } ->
        Mix.shell.info "  #{dep} : v#{version}"
      end)
    end
  end
end
