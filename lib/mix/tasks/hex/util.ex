defmodule Mix.Tasks.Hex.Util do
  def generate_key(username, password) do
    Mix.shell.info("Generating new api key...")
    { :ok, name } = :inet.gethostname()
    name = String.from_char_list!(name)

    case Hex.API.new_key(name, [user: username, pass: password]) do
      { 201, body } ->
        Mix.shell.info("New key generated: #{body["name"]}")
        Hex.Mix.update_config([username: username, key: body["secret"]])
      { code, body } ->
        Mix.shell.error("Generation of api key was failed! (#{code})")
        print_error_result(code, body)
    end
  end

  def update_config(config) do
    if Mix.shell.yes?("Update config file?") do
      Hex.Mix.update_config(config)
    end
  end

  def auth_opts(opts, user_config) do
    cond do
      user = opts[:user] ->
        unless pass = opts[:pass] do
          raise Mix.Error, message: "--pass option required if --user was given"
        end
        [user: user, pass: pass]

      key = user_config[:key] ->
        [key: key]

      true ->
        raise Mix.Error, message: "No user in config or given as command line argument"
    end
  end

  def required_opts(opts, required) do
    Enum.map(required, fn req ->
      unless Keyword.has_key?(opts, req) do
        raise Mix.Error, message: "Missing command line option: #{req}"
      end
    end)
  end

  # TODO: Move printing to Hex namespace

  def print_error_result(:http_error, reason) do
    Mix.shell.info(inspect(reason))
  end

  def print_error_result(_status, nil), do: :ok
  def print_error_result(_status, ""), do: :ok

  def print_error_result(_status, body) do
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
