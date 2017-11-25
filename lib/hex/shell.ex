defmodule Hex.Shell do
  def info(output) do
    Mix.shell.info(output)
  end

  def warn(output) do
    Mix.shell.info([IO.ANSI.yellow, output, IO.ANSI.reset])
  end

  def error(output) do
    Mix.shell.error(output)
  end

  def debug(output) do
    if function_exported?(Mix, :debug?, 0) and Mix.debug?() do
      info(output)
    end
  end

  def yes?(output) do
    Mix.shell.yes?(output)
  end

  def prompt(output) do
    Mix.shell.prompt(output)
  end
end
