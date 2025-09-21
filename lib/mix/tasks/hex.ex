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
    Hex.Shell.info("Starting OAuth device flow authentication...")

    case Hex.API.OAuth.device_authorization() do
      {:ok, {200, _, device_response}} ->
        perform_device_flow(device_response)

      {:ok, {status, _, error}} ->
        Hex.Shell.error("Device authorization failed (#{status}): #{inspect(error)}")
        :error

      {:error, reason} ->
        Hex.Shell.error("Device authorization error: #{inspect(reason)}")
        :error
    end
  end

  defp perform_device_flow(device_response) do
    device_code = device_response["device_code"]
    user_code = device_response["user_code"]
    verification_uri = device_response["verification_uri"]
    interval = device_response["interval"] || 5

    Hex.Shell.info("To authenticate, visit: #{verification_uri}")
    Hex.Shell.info("And enter the code: #{user_code}")
    Hex.Shell.info("")
    Hex.Shell.info("Waiting for authentication...")

    case poll_for_token(device_code, interval) do
      {:ok, initial_token} ->
        exchange_and_store_tokens(initial_token)

      :error ->
        :error
    end
  end

  defp poll_for_token(device_code, interval, attempt \\ 1) do
    case Hex.API.OAuth.poll_device_token(device_code) do
      {:ok, {200, _, token_response}} ->
        {:ok, token_response}

      {:ok, {400, _, %{"error" => "authorization_pending"}}} ->
        if attempt > 120 do
          Hex.Shell.error("Authentication timed out. Please try again.")
          :error
        else
          Process.sleep(interval * 1000)
          poll_for_token(device_code, interval, attempt + 1)
        end

      {:ok, {400, _, %{"error" => "slow_down"}}} ->
        # Increase polling interval
        new_interval = min(interval * 2, 30)
        Process.sleep(new_interval * 1000)
        poll_for_token(device_code, new_interval, attempt + 1)

      {:ok, {400, _, %{"error" => "expired_token"}}} ->
        Hex.Shell.error("Device code expired. Please try again.")
        :error

      {:ok, {403, _, %{"error" => "access_denied"}}} ->
        Hex.Shell.error("Authentication was denied.")
        :error

      {:ok, {status, _, error}} ->
        Hex.Shell.error("Authentication failed (#{status}): #{inspect(error)}")
        :error

      {:error, reason} ->
        Hex.Shell.error("Authentication error: #{inspect(reason)}")
        :error
    end
  end

  defp exchange_and_store_tokens(initial_token) do
    Hex.Shell.info("Authentication successful! Exchanging tokens...")

    # Exchange for write token
    case Hex.API.OAuth.exchange_token(initial_token["access_token"], "api:write") do
      {:ok, {200, _, write_token_response}} ->
        # Exchange for read token
        case Hex.API.OAuth.exchange_token(initial_token["access_token"], "api:read repositories") do
          {:ok, {200, _, read_token_response}} ->
            # Store both tokens
            tokens = %{
              "write" => Hex.OAuth.create_token_data(write_token_response),
              "read" => Hex.OAuth.create_token_data(read_token_response)
            }

            Hex.OAuth.store_tokens(tokens)
            Hex.Shell.info("Authentication completed successfully!")
            {:ok, tokens}

          {:ok, {status, _, error}} ->
            Hex.Shell.error("Failed to exchange read token (#{status}): #{inspect(error)}")
            :error

          {:error, reason} ->
            Hex.Shell.error("Read token exchange error: #{inspect(reason)}")
            :error
        end

      {:ok, {status, _, error}} ->
        Hex.Shell.error("Failed to exchange write token (#{status}): #{inspect(error)}")
        :error

      {:error, reason} ->
        Hex.Shell.error("Write token exchange error: #{inspect(reason)}")
        :error
    end
  end

  @doc false
  def generate_organization_key(organization_name, key_name, permissions, auth \\ nil) do
    auth = auth || auth_info(:write)

    case Hex.API.Key.Organization.new(organization_name, key_name, permissions, auth) do
      {:ok, {201, _, body}} ->
        {:ok, body["secret"]}

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
  def clear_oauth_tokens do
    Hex.OAuth.clear_tokens()
    Hex.Shell.info("OAuth tokens cleared.")
  end

  @doc false
  def update_keys(write_key, read_key \\ nil) do
    Hex.Config.update(
      "$write_key": write_key,
      "$read_key": read_key
    )

    Hex.State.put(:api_key_write, write_key)
    Hex.State.put(:api_key_read, read_key)
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
  def auth_info(permission, opts \\ [])

  def auth_info(:write, opts) do
    # Try OAuth tokens first
    case Hex.OAuth.get_token(:write) do
      {:ok, access_token} ->
        [key: access_token, oauth: true]

      {:error, :refresh_failed} ->
        Hex.Shell.info("Token refresh failed. Please re-authenticate.")

        if Keyword.get(opts, :auth_inline, true) do
          authenticate_inline()
        else
          []
        end

      {:error, :token_expired} ->
        Hex.Shell.info("Access token expired and could not be refreshed. Please re-authenticate.")

        if Keyword.get(opts, :auth_inline, true) do
          authenticate_inline()
        else
          []
        end

      {:error, :no_auth} ->
        # Fall back to API key from config/env or test setup
        case Hex.State.fetch!(:api_key_write_unencrypted) do
          nil ->
            # Also check regular API key (used by tests)
            case Hex.State.fetch!(:api_key_write) do
              nil ->
                if Keyword.get(opts, :auth_inline, true) do
                  authenticate_inline()
                else
                  []
                end

              api_key ->
                [key: api_key]
            end

          api_key ->
            [key: api_key]
        end
    end
  end

  def auth_info(:read, opts) do
    # Try OAuth tokens first
    case Hex.OAuth.get_token(:read) do
      {:ok, access_token} ->
        [key: access_token, oauth: true]

      {:error, :refresh_failed} ->
        Hex.Shell.info("Token refresh failed. Please re-authenticate.")

        if Keyword.get(opts, :auth_inline, true) do
          authenticate_inline()
        else
          []
        end

      {:error, :token_expired} ->
        Hex.Shell.info("Access token expired and could not be refreshed. Please re-authenticate.")

        if Keyword.get(opts, :auth_inline, true) do
          authenticate_inline()
        else
          []
        end

      {:error, :no_auth} ->
        # Fall back to API key from config/env or test setup
        case Hex.State.fetch!(:api_key_write_unencrypted) do
          nil ->
            # Also check regular API key (used by tests)
            case Hex.State.fetch!(:api_key_read) || Hex.State.fetch!(:api_key_write) do
              nil ->
                if Keyword.get(opts, :auth_inline, true) do
                  authenticate_inline()
                else
                  []
                end

              api_key ->
                [key: api_key]
            end

          api_key ->
            [key: api_key]
        end
    end
  end

  defp authenticate_inline() do
    authenticate? =
      Hex.Shell.yes?("No authenticated user found. Do you want to authenticate now?")

    if authenticate? do
      case auth() do
        {:ok, _tokens} ->
          # Auth succeeded, try to get write token
          case Hex.OAuth.get_token(:write) do
            {:ok, access_token} -> [key: access_token, oauth: true]
            {:error, _} -> no_auth_error()
          end

        :error ->
          no_auth_error()
      end
    else
      no_auth_error()
    end
  end

  defp no_auth_error() do
    Mix.raise("No authenticated user found. Run `mix hex.user auth`")
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

  if Mix.env() == :test do
    @doc false
    def set_exit_code(code), do: throw({:exit_code, code})
  else
    @doc false
    def set_exit_code(code), do: System.at_exit(fn _ -> System.halt(code) end)
  end
end
