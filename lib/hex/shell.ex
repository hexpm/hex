defmodule Hex.Shell do
  @moduledoc false

  def info(output) do
    validate_output!(output)
    Mix.shell().info(output)
  end

  def warn(output) do
    validate_output!(output)
    Mix.shell().info([IO.ANSI.yellow(), output, IO.ANSI.reset()])
  end

  def error(output) do
    validate_output!(output)
    Mix.shell().error(output)
  end

  def debug(output) do
    validate_output!(output)

    if Hex.Mix.debug?() do
      info(output)
    end
  end

  def yes?(output) do
    validate_output!(output)
    Mix.shell().yes?(output)
  end

  def prompt(output) do
    validate_output!(output)
    Mix.shell().prompt(output)
  end

  def format(output, emit? \\ Hex.Shell.ansi_enabled?()) do
    IO.ANSI.format(output, emit?)
  end

  def cmd(command, options \\ [], callback) when is_function(callback, 1) do
    callback =
      if Keyword.get(options, :quiet, false) do
        fn x -> x end
      else
        callback
      end

    env = validate_env(Keyword.get(options, :env, []))

    args =
      if Keyword.get(options, :stderr_to_stdout, true) do
        [:stderr_to_stdout]
      else
        []
      end

    opts = [:stream, :binary, :exit_status, :hide, :use_stdio, {:env, env} | args]
    port = Port.open({:spawn, shell_command(command)}, opts)
    port_read(port, callback)
  end

  defp port_read(port, callback) do
    receive do
      {^port, {:data, data}} ->
        _ = callback.(data)
        port_read(port, callback)

      {^port, {:exit_status, status}} ->
        status
    end
  end

  # Finding shell command logic from :os.cmd in OTP
  # https://github.com/erlang/otp/blob/8deb96fb1d017307e22d2ab88968b9ef9f1b71d0/lib/kernel/src/os.erl#L184
  defp shell_command(command) do
    case :os.type() do
      {:unix, _} ->
        command =
          command
          |> String.replace("\"", "\\\"")
          |> String.to_charlist()

        'sh -c "' ++ command ++ '"'

      {:win32, osname} ->
        command = '"' ++ String.to_charlist(command) ++ '"'

        case {System.get_env("COMSPEC"), osname} do
          {nil, :windows} -> 'command.com /s /c ' ++ command
          {nil, _} -> 'cmd /s /c ' ++ command
          {cmd, _} -> '#{cmd} /s /c ' ++ command
        end
    end
  end

  defp validate_env(enum) do
    Enum.map(enum, fn
      {k, nil} ->
        {String.to_charlist(k), false}

      {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}

      other ->
        raise ArgumentError, "invalid environment key-value #{inspect(other)}"
    end)
  end

  if Mix.env() == :test do
    defp validate_output!(output) do
      formatted_output = output |> IO.ANSI.format_fragment(true) |> IO.chardata_to_string()

      unless String.printable?(formatted_output) do
        raise ArgumentError, "string not printable"
      end
    end
  else
    defp validate_output!(_output), do: :ok
  end

  if Mix.env() == :test do
    def ansi_enabled?(), do: false
  else
    def ansi_enabled?(), do: IO.ANSI.enabled?()
  end
end
