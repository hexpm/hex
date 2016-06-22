defmodule Mix.Hex.Utils do
  def table(header, values) do
    header = Enum.map(header, &[:underline, &1])
    widths = widths([header|values])

    print_row(header, widths)
    Enum.each(values, &print_row(&1, widths))
  end

  defp ansi_length(binary) when is_binary(binary),
    do: byte_size(binary)
  defp ansi_length(list) when is_list(list),
    do: Enum.reduce(list, 0, &(ansi_length(&1) + &2))
  defp ansi_length(atom) when is_atom(atom),
    do: 0

  defp print_row(strings, widths) do
    Enum.map(Enum.zip(strings, widths), fn {string, width} ->
      pad_size = width-ansi_length(string)+2
      pad = :lists.duplicate(pad_size, ?\s)
      [string, :reset, pad]
    end)
    |> IO.ANSI.format
    |> Hex.Shell.info
  end

  defp widths([head|tail]) do
    widths = Enum.map(head, &ansi_length/1)

    Enum.reduce(tail, widths, fn list, acc ->
      Enum.zip(list, acc)
      |> Enum.map(fn {string, width} -> max(width, ansi_length(string)) end)
    end)
  end

  def generate_key(username, password) do
    Hex.Shell.info("Generating API key...")
    {:ok, name} = :inet.gethostname()
    name = List.to_string(name)

    case Hex.API.Key.new(name, [user: username, pass: password]) do
      {201, body, _} ->
        Hex.Config.update([username: username, key: body["secret"]])
      {code, body, _} ->
        Mix.shell.error("Generation of API key failed (#{code})")
        Hex.Utils.print_error_result(code, body)
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
  def password_get(prompt, false) do
    IO.gets(prompt <> " ")
  end
  def password_get(prompt, true) do
    pid   = spawn_link(fn -> loop(prompt) end)
    ref   = make_ref()
    value = IO.gets(prompt <> " ")

    send pid, {:done, self(), ref}
    receive do: ({:done, ^pid, ^ref}  -> :ok)

    value
  end

  defp loop(prompt) do
    receive do
      {:done, parent, ref} ->
        send parent, {:done, self(), ref}
        IO.write(:standard_error, "\e[2K\r")
    after
      1 ->
        IO.write(:standard_error, "\e[2K\r#{prompt} ")
        loop(prompt)
    end
  end

  @progress_steps 25

  def progress(nil) do
    fn _ -> nil end
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

  def clean_version("v" <> version), do: version
  def clean_version(version),        do: version
end
