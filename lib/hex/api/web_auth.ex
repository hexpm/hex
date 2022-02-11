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

    fn -> access_key_task(%{device_code: device_code}, DateTime.utc_now()) end
    |> Task.async()
    |> Task.await(5 * 60 * 1000)
  end

  defp access_key_task(params, last_request_time) do
    case API.erlang_post_request(nil, "web_auth/access_key", params) do
      {:ok, {_code, %{"write_key" => write_key, "read_key" => read_key}, _headers}} ->
        %{write_key: write_key, read_key: read_key}

      {:ok, {_code, %{"message" => "request to be verified"}, _headers}} ->
        diff = DateTime.diff(DateTime.utc_now(), last_request_time)

        if diff < 1000 do
          Process.sleep(1000 - diff)
        end

        access_key_task(params, DateTime.utc_now())
    end
  end
end
