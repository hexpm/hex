defmodule Hex.API.WebAuth do
  @moduledoc false

  alias Hex.API

  def get_code(key_name) do
    {:ok, {_status, code, _}} =
      API.erlang_post_request(nil, "web_auth/code", %{key_name: key_name})

    code
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
        Process.sleep(1000)
        access_key_task(params)
    end
  end
end
