defmodule Mix.Tasks.Hex.Util do
  def generate_key(username, password) do
    Mix.shell.info("Generating API key...")
    {:ok, name} = :inet.gethostname()
    name = List.to_string(name)

    case Hex.API.new_key(name, [user: username, pass: password]) do
      {201, body} ->
        Hex.Mix.update_config([username: username, key: body["secret"]])
      {code, body} ->
        Mix.shell.error("Generation of API key failed (#{code})")
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
          Mix.raise "--pass option required if --user was given"
        end
        [user: user, pass: pass]

      key = user_config[:key] ->
        [key: key]

      true ->
        Mix.raise "No user in config or given as command line argument"
    end
  end

  def required_opts(opts, required) do
    Enum.map(required, fn req ->
      unless Keyword.has_key?(opts, req) do
        Mix.raise "Missing command line option: #{req}"
      end
    end)
  end

  # Password prompt that hides input by every 1ms
  # clearing the line with stderr
  def password_get(prompt, clean?) do
    if clean? do
      pid = spawn_link fn -> loop(prompt) end
      ref = make_ref()
    end

    value = IO.gets(prompt <> " ")

    if clean? do
      send pid, {:done, self(), ref}
      receive do: ({:done, ^pid, ^ref}  -> :ok)
    end

    value
  end

  defp loop(prompt) do
    receive do
      {:done, parent, ref} ->
        send parent, {:done, self, ref}
        IO.write :standard_error, "\e[2K\r"
    after
      1 ->
        IO.write :standard_error, "\e[2K\r#{prompt} "
        loop(prompt)
    end
  end
end
