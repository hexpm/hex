defmodule Mix.Tasks.Hex.Util do
  def generate_key(username, password) do
    Mix.shell.info("Generating API key...")
    {:ok, name} = :inet.gethostname()
    name = List.to_string(name)

    case Hex.API.Key.new(name, [user: username, pass: password]) do
      {201, body} ->
        Hex.Config.update([username: username, key: body["secret"]])
      {code, body} ->
        Mix.shell.error("Generation of API key failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  def auth_info(config \\ Hex.Config.read) do
    if key = config[:key] do
      [key: key]
    else
      Mix.raise "No authorized user found. Run 'mix hex.user auth'"
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

  @progress_steps 25

  def progress(nil) do
    fn _ -> end
  end

  def progress(max) do
    put_progress(0, 0)

    fn size ->
      fraction = size / max
      completed = trunc(fraction * @progress_steps)
      put_progress(completed, trunc(fraction * 100))
      size
    end
  end

  defp put_progress(completed, percent) do
    unfilled = @progress_steps - completed
    str = "\r[#{String.duplicate("#", completed)}#{String.duplicate(" ", unfilled)}]"
    IO.write(:stderr, str <> " #{percent}%")
  end

  def spin(interval) do
    chars = ["|", "/", "-", "\\"]

    chars |> Enum.map(fn c ->
      IO.write(:stderr, "\r#{c}")
      :timer.sleep(interval)
    end)

    spin(interval)
  end

  def start_spinner(interval \\ 100) do
    %Task{pid: pid} = Task.async(__MODULE__, :spin, [interval])

    pid
  end

  def stop_spinner(pid) do
    Process.exit(pid, :normal) # kill the process
    IO.write(:stderr, "\r") # clear line
  end

  def clean_version("v" <> version), do: version
  def clean_version(version),        do: version
end
