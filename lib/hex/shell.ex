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

    if Hex.debug?() do
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
    def ansi_enabled?(), do: true
  end
end
