defmodule Mix.Tasks.Explex.Util do
  def required_opts(opts, required) do
    Enum.map(required, fn req ->
      unless Keyword.has_key?(opts, req) do
        raise Mix.Error, message: "Missing command line option: #{req}"
      end
    end)
  end

  def print_error_result(nil), do: :ok

  def print_error_result(body) do
    if body["message"] && body["errors"] do
      Mix.shell.info(body["message"])
      pretty_errors(body["errors"])
    else
      Mix.shell.info(inspect(body))
    end
  end

  defp pretty_errors(errors, depth \\ 0) do
    Enum.each(errors, fn
      { key, list } when is_list(list) ->
        Mix.shell.info(indent(depth) <> key <> ":")
        pretty_errors(errors, depth + 1)
      { key, value } ->
        Mix.shell.info(indent(depth) <> key <> ": " <> value)
    end)
  end

  defp indent(0), do: "  "
  defp indent(depth), do: "  " <> indent(depth - 1)
end
