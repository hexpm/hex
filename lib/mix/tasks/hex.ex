defmodule Mix.Tasks.Hex do
  use Mix.Task

  @shortdoc "Prints Hex help information"

  @moduledoc """
  Prints Hex tasks and their information.

      $ mix hex

  See `mix help hex.config` to see all available configuration options.
  """

  @impl true
  def run(_args) do
    Hex.start()

    Hex.Shell.info("Hex v" <> Hex.version())
    Hex.Shell.info("Hex is a package manager for the Erlang ecosystem.")

    print_available_tasks()

    Hex.Shell.info("Further information can be found here: https://hex.pm/docs")
  end

  defp print_available_tasks() do
    line_break()
    Hex.Shell.info("Available tasks:")
    line_break()

    pattern = "hex."
    modules = Enum.filter(load_tasks(), &String.contains?(Mix.Task.task_name(&1), pattern))
    {docs, max} = build_task_doc_list(modules)
    display_doc_list(docs, max)
    line_break()
  end

  defp load_tasks() do
    Enum.filter(Mix.Task.load_all(), &(Mix.Task.moduledoc(&1) != false))
  end

  defp build_task_doc_list(modules) do
    Enum.reduce(modules, {[], 0}, fn module, {docs, max} ->
      task = "mix " <> Mix.Task.task_name(module)

      task_list =
        if Keyword.has_key?(module.__info__(:functions), :tasks) do
          Enum.map(module.tasks(), fn {subtask, docs} -> {"#{task} #{subtask}", docs} end)
        else
          []
        end

      max =
        Enum.reduce(task_list, max, fn {task, _}, max_now ->
          max(byte_size(task), max_now)
        end)

      if Enum.empty?(task_list) do
        {docs, max}
      else
        {docs ++ [task_list], max}
      end
    end)
  end

  defp display_doc_list(docs, max) do
    Enum.each(Enum.sort_by(docs, &List.first/1), fn tasks ->
      Enum.each(tasks, fn {task, doc} ->
        Mix.shell().info(format_task(task, max, doc))
      end)

      line_break()
    end)
  end

  defp format_task(task, max, doc) do
    String.pad_trailing(task, max) <> " # " <> doc
  end

  defp line_break(), do: Hex.Shell.info("")

  @doc false
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

  @doc false
  def auth(opts \\ []) do
    auth_device(opts)
  end

  @doc false
  def auth_device(_opts \\ []) do
    # Clean up any existing authentication
    revoke_existing_oauth_tokens()
    revoke_and_cleanup_old_api_keys()

    prompt_user = fn verification_uri, user_code ->
      Hex.Shell.info("To authenticate, visit: #{verification_uri}")
      Hex.Shell.info("")
      Hex.Shell.info("Your verification code:")
      Hex.Shell.info("")
      Hex.Shell.info("  #{format_user_code(user_code)}")
      Hex.Shell.info("")
      Hex.Shell.info("Verify this code matches what is shown in your browser.")
      Hex.Shell.info("")
      Hex.Shell.info("Waiting for authentication...")
      :ok
    end

    case Hex.API.OAuth.device_auth_flow("api repositories", prompt_user, open_browser: true) do
      {:ok, tokens} ->
        store_token(tokens)

      {:error, :timeout} ->
        Hex.Shell.error("Device code expired. Please try again.")
        :error

      {:error, {:access_denied, _status, _body}} ->
        Hex.Shell.error("Authentication was denied.")
        :error

      {:error, {:device_auth_failed, status, body}} ->
        Hex.Shell.error("Device authorization failed (#{status}): #{inspect(body)}")
        :error

      {:error, {:poll_failed, status, body}} ->
        Hex.Shell.error("Authentication failed (#{status}): #{inspect(body)}")
        :error

      {:error, reason} ->
        Hex.Shell.error("Authentication error: #{inspect(reason)}")
        :error
    end
  end

  defp format_user_code(user_code) do
    mid = div(String.length(user_code), 2)

    String.slice(user_code, 0, mid) <>
      "-" <> String.slice(user_code, mid, String.length(user_code))
  end

  defp store_token(tokens) do
    Hex.OAuth.store_token(tokens)
    Hex.Shell.info("You are authenticated!")
    {:ok, tokens}
  end

  @doc false
  def generate_organization_key(organization_name, key_name, permissions) do
    case Hex.API.Key.Organization.new(organization_name, key_name, permissions) do
      {:ok, {201, _, body}} ->
        {:ok, body["secret"]}

      {:error, {:auth_error, _}} ->
        Mix.shell().error("Generation of key failed: authentication required")
        :error

      other ->
        Mix.shell().error("Generation of key failed")
        Hex.Utils.print_error_result(other)
        :error
    end
  end

  @doc false
  def general_key_name(nil) do
    {:ok, hostname} = :inet.gethostname()
    List.to_string(hostname)
  end

  def general_key_name(key) do
    key
  end

  @doc false
  def api_key_name(key, extra \\ nil) do
    {:ok, hostname} = :inet.gethostname()
    name = "#{key || hostname}-api"
    if extra, do: "#{name}-#{extra}", else: name
  end

  @doc false
  def repository_key_name(organization, key) do
    {:ok, hostname} = :inet.gethostname()
    "#{key || hostname}-repository-#{organization}"
  end

  @doc false
  def repositories_key_name(key) do
    {:ok, hostname} = :inet.gethostname()
    "#{key || hostname}-repositories"
  end

  @doc false
  def revoke_existing_oauth_tokens do
    case Hex.Config.read()[:"$oauth_token"] do
      nil ->
        :ok

      token_data when is_map(token_data) ->
        if access_token = token_data["access_token"] do
          case Hex.API.OAuth.revoke_token(access_token) do
            {:ok, {code, _, _}} when code in 200..299 ->
              :ok

            _ ->
              :ok
          end
        end

        Hex.Config.remove([:"$oauth_token"])

      _ ->
        :ok
    end
  end

  @doc false
  def revoke_and_cleanup_old_api_keys do
    config = Hex.Config.read()

    # Check for old write key
    if write_key = config[:"$write_key"] do
      # Try to revoke on server (might fail if already revoked or invalid)
      _ = Hex.API.Key.delete(write_key, key: write_key)
    end

    # Check for old read key (only if different from write key)
    if read_key = config[:"$read_key"] do
      if read_key != config[:"$write_key"] do
        _ = Hex.API.Key.delete(read_key, key: read_key)
      end
    end

    # Remove from config if they existed
    if config[:"$write_key"] || config[:"$read_key"] do
      Hex.Config.remove([:"$write_key", :"$read_key"])
    end
  end

  @doc false
  def auth_organization(name, key) do
    repo = Hex.Repo.get_repo(name) || Hex.Repo.default_hexpm_repo()
    repo = Map.put(repo, :auth_key, key)

    Hex.State.fetch!(:repos)
    |> Map.put(name, repo)
    |> Hex.Config.update_repos()
  end

  @doc false
  def required_opts(opts, required) do
    Enum.map(required, fn req ->
      unless Keyword.has_key?(opts, req) do
        Mix.raise("Missing command line option: #{req}")
      end
    end)
  end

  @doc false
  def convert_permissions([]) do
    nil
  end

  @doc false
  def convert_permissions(permissions) do
    Enum.map(permissions, fn permission ->
      permission = String.downcase(permission)
      destructure [domain, resource], String.split(permission, ":", parts: 2)
      %{"domain" => domain, "resource" => resource}
    end)
  end

  @progress_steps 25

  @doc false
  def progress(nil) do
    fn _ -> nil end
  end

  def progress(max) do
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

  if Mix.env() == :test do
    @doc false
    def set_exit_code(code), do: throw({:exit_code, code})
  else
    @doc false
    def set_exit_code(code), do: System.at_exit(fn _ -> System.halt(code) end)
  end
end
