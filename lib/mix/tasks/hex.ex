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

    Hex.Shell.info("Hex v" <> Hex.version())
    Hex.Shell.info("Hex is a package manager for the Erlang ecosystem.")
    line_break()

    if Hex.Version.match?(System.version(), ">= 1.1.0-dev") do
      Hex.Shell.info("Available tasks:")
      line_break()
      Mix.Task.run("help", ["--search", "hex."])
      line_break()
    end

    Hex.Shell.info("Further information can be found here: https://hex.pm/docs/tasks")
  end

  def line_break(), do: Hex.Shell.info("")

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

  def auth(opts \\ []) do
    username = Hex.Shell.prompt("Username:") |> Hex.string_trim()
    account_password = Mix.Tasks.Hex.password_get("Account password:") |> Hex.string_trim()
    Mix.Tasks.Hex.generate_all_user_keys(username, account_password, opts)
  end

  @local_password_prompt "You have authenticated on Hex using your account password. However, " <>
                           "Hex requires you to have a local password that applies only to this machine for security " <>
                           "purposes. Please enter it."

  def generate_user_key(key_name, permissions, opts) do
    case Hex.API.Key.new(key_name, permissions, opts) do
      {:ok, {201, body, _}} ->
        {:ok, body["secret"]}

      other ->
        Mix.shell().error("Generation of key failed")
        Hex.Utils.print_error_result(other)
        :error
    end
  end

  def generate_all_user_keys(username, password, opts \\ []) do
    Hex.Shell.info("Generating keys...")
    auth = [user: username, pass: password]
    key_name = api_key_name(opts[:key_name])
    permissions = [%{"domain" => "api"}]

    case generate_user_key(key_name, permissions, auth) do
      {:ok, write_key} ->
        key_name = api_key_name(opts[:key_name], "read")
        permissions = [%{"domain" => "api", "resource" => "read"}]

        case generate_user_key(key_name, permissions, key: write_key) do
          {:ok, read_key} ->
            key_name = repositories_key_name(opts[:key_name])
            permissions = [%{"domain" => "repositories"}]

            case generate_user_key(key_name, permissions, key: write_key) do
              {:ok, organization_key} ->
                auth_organization("hexpm", organization_key)

                Hex.Shell.info(@local_password_prompt)
                prompt_encrypt_key(write_key, read_key)
                {:ok, write_key, read_key, organization_key}

              :error ->
                :ok
            end

          :error ->
            :error
        end

      :error ->
        :error
    end
  end

  def generate_organization_key(organization_name, key_name, permissions, auth \\ nil) do
    auth = auth || auth_info(:write)

    case Hex.API.Key.Organization.new(organization_name, key_name, permissions, auth) do
      {:ok, {201, body, _}} ->
        {:ok, body["secret"]}

      other ->
        Mix.shell().error("Generation of key failed")
        Hex.Utils.print_error_result(other)
        :error
    end
  end

  def general_key_name(nil) do
    {:ok, hostname} = :inet.gethostname()
    List.to_string(hostname)
  end

  def general_key_name(key) do
    key
  end

  def api_key_name(key, extra \\ nil) do
    {:ok, hostname} = :inet.gethostname()
    name = "#{key || hostname}-api"
    if extra, do: "#{name}-#{extra}", else: name
  end

  def repository_key_name(organization, key) do
    {:ok, hostname} = :inet.gethostname()
    "#{key || hostname}-repository-#{organization}"
  end

  def repositories_key_name(key) do
    {:ok, hostname} = :inet.gethostname()
    "#{key || hostname}-repositories"
  end

  def update_keys(write_key, read_key \\ nil) do
    Hex.Config.update(
      "$write_key": write_key,
      "$read_key": read_key,
      "$encrypted_key": nil,
      encrypted_key: nil
    )

    Hex.State.put(:api_key_write, write_key)
    Hex.State.put(:api_key_read, read_key)
  end

  def auth_organization(name, key) do
    repo = Hex.Repo.get_repo(name) || Hex.Repo.default_hexpm_repo()
    repo = Map.put(repo, :auth_key, key)

    Hex.State.fetch!(:repos)
    |> Map.put(name, repo)
    |> Hex.Config.update_repos()
  end

  def auth_info(permission, opts \\ [])

  def auth_info(:write, opts) do
    api_key_write_unencrypted = Hex.State.fetch!(:api_key_write_unencrypted)
    api_key_write = Hex.State.fetch!(:api_key_write)

    cond do
      api_key_write_unencrypted -> [key: api_key_write_unencrypted]
      api_key_write -> [key: prompt_decrypt_key(api_key_write)]
      Keyword.get(opts, :auth_inline, true) -> authenticate_inline()
      true -> []
    end
  end

  def auth_info(:read, opts) do
    api_key_write_unencrypted = Hex.State.fetch!(:api_key_write_unencrypted)
    api_key_read = Hex.State.fetch!(:api_key_read)

    cond do
      api_key_write_unencrypted -> [key: api_key_write_unencrypted]
      api_key_read -> [key: api_key_read]
      Keyword.get(opts, :auth_inline, true) -> authenticate_inline()
      true -> []
    end
  end

  defp authenticate_inline() do
    authenticate? =
      Hex.Shell.yes?("No authenticated user found. Do you want to authenticate now?")

    if authenticate? do
      case auth() do
        {:ok, write_key, _read_key, _org_key} -> [key: write_key]
        :error -> no_auth_error()
      end
    else
      no_auth_error()
    end
  end

  defp no_auth_error() do
    Mix.raise("No authenticated user found. Run `mix hex.user auth`")
  end

  def prompt_encrypt_key(write_key, read_key, challenge \\ "Local password") do
    password = password_get("#{challenge}:") |> Hex.string_trim()
    confirm = password_get("#{challenge} (confirm):") |> Hex.string_trim()

    if password != confirm do
      Hex.Shell.error("Entered passwords do not match. Try again")
      prompt_encrypt_key(write_key, read_key, challenge)
    end

    encrypted_write_key = Hex.Crypto.encrypt(write_key, password, @apikey_tag)
    update_keys(encrypted_write_key, read_key)
  end

  def prompt_decrypt_key(encrypted_key, challenge \\ "Local password") do
    password = password_get("#{challenge}:") |> Hex.string_trim()

    case Hex.Crypto.decrypt(encrypted_key, password, @apikey_tag) do
      {:ok, key} ->
        key

      :error ->
        Hex.Shell.error("Wrong password. Try again")
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
        Mix.raise("Missing command line option: #{req}")
      end
    end)
  end

  def convert_permissions([]) do
    nil
  end

  def convert_permissions(permissions) do
    Enum.map(permissions, fn permission ->
      permission = String.downcase(permission)
      destructure [domain, resource], String.split(permission, ":", parts: 2)
      %{"domain" => domain, "resource" => resource}
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

    send(pid, {:done, self(), ref})
    receive do: ({:done, ^pid, ^ref} -> :ok)

    value
  end

  defp loop(prompt) do
    receive do
      {:done, parent, ref} ->
        send(parent, {:done, self(), ref})
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
    def set_exit_code(code), do: throw({:exit_code, code})
  else
    def set_exit_code(code), do: System.at_exit(fn _ -> System.halt(code) end)
  end
end
