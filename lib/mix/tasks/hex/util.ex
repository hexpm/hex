defmodule Mix.Tasks.Hex.Util do
  def generate_key(username, password) do
    Mix.shell.info("Generating api key...")
    { :ok, name } = :inet.gethostname()
    name = String.from_char_data!(name)

    case Hex.API.new_key(name, [user: username, pass: password]) do
      { 201, body } ->
        Hex.Mix.update_config([username: username, key: body["secret"]])
      { code, body } ->
        Mix.shell.error("Generation of api key was failed (#{code})")
        Hex.Util.print_error_result(code, body)
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
end
