defmodule Hex.API.WebAuth do
  @moduledoc false

  alias Hex.API

  def get_code(key_name) do
    API.request(:post, nil, "login/web_auth/code", %{key_name: key_name})
  end

  def submit_in_browser() do
    Task.start(fn ->
      "Open link in browser?"
      |> Hex.Shell.format()
      |> Hex.Shell.yes?()
      |> open("https://hex.pm/login/web_auth")
    end)
  end

  def access_key(device_code) do
    Hex.API.check_write_api()

    fn x -> access_key_task(x) end
    |> Task.async()
    |> Task.await(%{device_code: device_code}, 5 * 60 * 1000)
  end

  defp access_key_task(params) do
    case API.request(:post, nil, "login/web_auth/access_key", params) do
      {:ok, {_code, %{"write_key" => write_key, "read_key" => read_key}, _headers}} ->
        %{write_key: write_key, read_key: read_key}

      {:ok, {_code, %{"error" => "request to be verified"}, _headers}} ->
        access_key_task(params)
    end
  end

  defp open(bool, url)
  defp open(false, _url), do: :ok

  defp open(true, url) do
    {cmd, args} =
      case :os.type() do
        {:unix, :darwin} -> {"open", [url]}
        {:unix, _} -> {"xdg-open", [url]}
        {:win32, _} -> {"cmd", ["/c", "start", url]}
      end

    System.cmd(cmd, args)
  end
end
