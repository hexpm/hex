defmodule Hex.Shell do
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

    if function_exported?(Mix, :debug?, 0) and Mix.debug?() do
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
end
