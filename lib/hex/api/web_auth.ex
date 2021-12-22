defmodule Hex.API.WebAuth do
  @moduledoc false

  alias Hex.API

  def get_code(key_name) do
    nil
    |> API.erlang_post_request("web_auth/code", %{key_name: key_name})
    |> elem(1)
    |> elem(1)
  end

  def submit_in_browser() do
    "Open link in browser?"
    |> Hex.Shell.yes?()
    |> open()
  end

  def access_key(device_code) do
    Hex.API.check_write_api()

    fn -> access_key_task(%{device_code: device_code}) end
    |> Task.async()
    |> Task.await(5 * 60 * 1000)
  end

  defp access_key_task(params) do
    case API.erlang_post_request(nil, "web_auth/access_key", params) do
      {:ok, {_code, %{"write_key" => write_key, "read_key" => read_key}, _headers}} ->
        %{write_key: write_key, read_key: read_key}

      {:ok, {_code, %{"message" => "request to be verified"}, _headers}} ->
        access_key_task(params)
    end
  end

  defp open(bool)
  defp open(false), do: :ok

  defp open(true) do
    url =
      :api_url
      |> Hex.State.fetch!()
      |> String.replace_suffix("api", "login/web_auth")

    {cmd, args} =
      case :os.type() do
        {:unix, :darwin} -> {"open", [url]}
        {:unix, _} -> {"xdg-open", [url]}
        {:win32, _} -> {"cmd", ["/c", "start", url]}
      end

    System.cmd(cmd, args)
  end
end
