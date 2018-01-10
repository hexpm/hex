defmodule Mix.Tasks.Hex do
  use Mix.Task

  @apikey_tag "HEXAPIKEY"

  @shortdoc "Prints Hex help information"

  @moduledoc """
  Prints Hex tasks and their information.

       mix hex

  See `mix help hex.config` to see all available configuration options.
  """

  def run(_args) do
    Hex.start()

    Hex.Shell.info "Hex v" <> Hex.version()
    Hex.Shell.info "Hex is a package manager for the Erlang ecosystem."
    line_break()

    if Hex.Version.match?(System.version(), ">= 1.1.0-dev") do
      Hex.Shell.info "Available tasks:"
      line_break()
      Mix.Task.run("help", ["--search", "hex."])
      line_break()
    end

    Hex.Shell.info "Further information can be found here: https://hex.pm/docs/tasks"
  end

  def line_break(), do: Hex.Shell.info ""

  def print_table(header, values) do
    header = Enum.map(header, &[:underline, &1])
    widths = widths([header | values])

    print_row(header, widths)
    Enum.each(values, &print_row(&1, widths))
  end

  defp ansi_length(binary) when is_binary(binary) do
    byte_size(binary)
  end
  defp ansi_length(list) when is_list(list) do
    Enum.reduce(list, 0, &(ansi_length(&1) + &2))
  end
  defp ansi_length(atom) when is_atom(atom) do
    0
  end

  defp print_row(strings, widths) do
    Enum.zip(strings, widths)
    |> Enum.map(fn {string, width} ->
      pad_size = width - ansi_length(string) + 2
      pad = :lists.duplicate(pad_size, ?\s)
      [string || "", :reset, pad]
    end)
    |> IO.ANSI.format()
    |> Hex.Shell.info()
  end

  defp widths([head | tail]) do
    widths = Enum.map(head, &ansi_length/1)

    Enum.reduce(tail, widths, fn list, acc ->
      Enum.zip(list, acc)
      |> Enum.map(fn {string, width} -> max(width, ansi_length(string)) end)
    end)
  end

  @local_password_prompt "You have authenticated on Hex using your account password. However, " <>
    "Hex requires you to have a local password that applies only to this machine for security " <>
    "purposes. Please enter it."

  def generate_api_key(username, password) do
    Hex.Shell.info("Generating API key...")
    {:ok, name} = :inet.gethostname()
    name = List.to_string(name)

    case Hex.API.Key.new(name, [user: username, pass: password]) do
      {:ok, {201, body, _}} ->
        Hex.Shell.info(@local_password_prompt)
        prompt_encrypt_key(body["secret"])

      other ->
        Mix.shell.error("Generation of API key failed")
        Hex.Utils.print_error_result(other)
        :error
    end
  end

  def generate_organization_key(name, auth \\ nil) do
    auth = auth || auth_info()
    permissions = [%{"domain" => "repository", "resource" => name}]

    {:ok, host} = :inet.gethostname()
    key = "#{host}-#{name}-organization"

    case Hex.API.Key.new(key, permissions, auth) do
      {:ok, {201, body, _}} ->
        body["secret"]
      other ->
        Hex.Utils.print_error_result(other)
        Mix.raise "Generation of organization key failed"
    end
  end

  def update_key(key) do
    Hex.Config.update(["$encrypted_key": key, encrypted_key: nil])
    Hex.State.put(:api_key, key)
  end

  def auth(opts \\ []) do
    username = Hex.Shell.prompt("Username:") |> Hex.string_trim()
    password = password_get("Account password:") |> Hex.string_trim()

    unless opts[:skip_organizations] do
      case generate_api_key(username, password) do
        {:ok, key, password} ->
          Hex.Shell.info("Generating organization keys...")
          auth = [key: key]
          {:ok, {200, user, _headers}} = Hex.API.User.me(auth)

          Enum.each(user["organizations"] || [], fn %{"name" => name} ->
            key = generate_organization_key(name, auth)
            auth_organization(name, key)
          end)

          {:ok, key, password}

        :error ->
          :ok
      end
    end
  end

  def auth_organization(name, key) do
    hexpm = Hex.Repo.get_repo("hexpm")
    repo = %{
      url: hexpm.url <> "/repos/#{name}",
      public_key: nil,
      auth_key: key
    }

    Hex.Config.read()
    |> Hex.Config.read_repos()
    |> Map.put("hexpm:#{name}", repo)
    |> Hex.Config.update_repos()
  end

  def auth_info() do
    cond do
      key = Hex.State.fetch!(:api_key) ->
        [key: prompt_decrypt_key(key)]

      Hex.Shell.yes?("No authenticated user found. Do you want to authenticate now?") ->
        if password = auth() do
          key = Hex.State.fetch!(:api_key)
          [key: decrypt_key(key, password)]
        else
          no_auth_error()
        end

      true ->
        no_auth_error()
    end
  end

  defp no_auth_error() do
    Mix.raise "No authenticated user found. Run `mix hex.user auth`"
  end

  def prompt_encrypt_key(key, challenge \\ "Local password") do
    password = password_get("#{challenge}:") |> Hex.string_trim()
    confirm = password_get("#{challenge} (confirm):") |> Hex.string_trim()
    if password != confirm do
      Hex.Shell.error "Entered passwords do not match. Try again"
      prompt_encrypt_key(key, challenge)
    end

    encrypted_key = Hex.Crypto.encrypt(key, password, @apikey_tag)
    update_key(encrypted_key)
    {:ok, key, password}
  end

  def prompt_decrypt_key(encrypted_key, challenge \\ "Local password") do
    password = password_get("#{challenge}:") |> Hex.string_trim()

    case Hex.Crypto.decrypt(encrypted_key, password, @apikey_tag) do
      {:ok, key} ->
        key
      :error ->
        Hex.Shell.error "Wrong password. Try again"
        prompt_decrypt_key(encrypted_key, challenge)
    end
  end

  def encrypt_key(password, key) do
    Hex.Crypto.encrypt(key, password, @apikey_tag)
  end

  def decrypt_key(password, key) do
    Hex.Crypto.decrypt(key, password, @apikey_tag)
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
  def password_get(prompt) do
    if Hex.State.fetch!(:clean_pass) do
      password_clean(prompt)
    else
      Hex.Shell.prompt(prompt <> " ")
    end
  end

  defp password_clean(prompt) do
    pid = spawn_link(fn -> loop(prompt) end)
    ref = make_ref()
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
  def clean_version(version), do: version

  if Mix.env() == :test do
    def set_exit_code(code), do: throw {:exit_code, code}
  else
    def set_exit_code(code), do: System.at_exit(fn(_) -> System.halt(code) end)
  end
end
