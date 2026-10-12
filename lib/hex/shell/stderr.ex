defmodule Hex.Shell.Stderr do
  @moduledoc false

  # Mix shell for tasks that write data to stdout. Messages, prompts and
  # command output are all printed to stderr.

  @behaviour Mix.Shell

  def print_app do
    if name = Mix.Shell.printable_app_name() do
      IO.puts(:stderr, "==> #{name}")
    end

    :ok
  end

  def info(message) do
    print_app()
    IO.puts(:stderr, IO.ANSI.format(message))
  end

  def error(message) do
    print_app()
    IO.puts(:stderr, IO.ANSI.format([:red, :bright, message]))
  end

  def prompt(message) do
    print_app()
    IO.write(:stderr, message <> " ")
    IO.gets("")
  end

  def yes?(message, options \\ []) do
    {prompt, answers} =
      case Keyword.get(options, :default, :yes) do
        :yes -> {" [Yn] ", ["", "y", "Y", "yes", "YES", "Yes"]}
        :no -> {" [yN] ", ["y", "Y", "yes", "YES", "Yes"]}
      end

    print_app()
    IO.write(:stderr, message <> prompt)
    answer = IO.gets("")
    is_binary(answer) and String.trim(answer) in answers
  end

  def cmd(command, opts \\ []) do
    print_app? = Keyword.get(opts, :print_app, true)

    Hex.Shell.cmd(command, opts, fn data ->
      if print_app?, do: print_app()
      IO.write(:stderr, data)
    end)
  end
end
