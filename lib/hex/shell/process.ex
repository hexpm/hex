defmodule Hex.Shell.Process do
  @moduledoc false

  @behaviour Mix.Shell

  def flush(callback \\ fn x -> x end) do
    receive do
      {:mix_shell, _, _} = message ->
        callback.(message)
        flush(callback)

      {:mix_shell_input, _, _} = message ->
        callback.(message)
        flush(callback)
    after
      0 -> :done
    end
  end

  def print_app do
    if name = Mix.Shell.printable_app_name() do
      send(process(), {:mix_shell, :info, ["==> #{name}"]})
    end
  end

  def info(message) do
    print_app()
    send(process(), {:mix_shell, :info, [format(message)]})
    :ok
  end

  def error(message) do
    print_app()
    send(process(), {:mix_shell, :error, [format(message)]})
    :ok
  end

  defp format(message) do
    message |> IO.ANSI.format(false) |> IO.iodata_to_binary()
  end

  def prompt(message) do
    print_app()
    send(process(), {:mix_shell, :prompt, [message]})

    receive do
      {:mix_shell_input, :prompt, response} -> response
    after
      0 -> raise "no shell process input given for prompt/1"
    end
  end

  def yes?(message, _options \\ []) do
    print_app()
    send(process(), {:mix_shell, :yes?, [message]})

    receive do
      {:mix_shell_input, :yes?, response} -> response
    after
      0 -> raise "no shell process input given for yes?/1"
    end
  end

  def cmd(command, opts \\ []) do
    print_app? = Keyword.get(opts, :print_app, true)

    Hex.Shell.cmd(command, opts, fn data ->
      if print_app?, do: print_app()
      send(process(), {:mix_shell, :run, [data]})
    end)
  end

  defp process() do
    Hex.State.fetch!(:shell_process) || self()
  end
end
