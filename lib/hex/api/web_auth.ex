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

    now = System.os_time(:millisecond)
    access_key(%{device_code: device_code}, now, now + 5 * 60 * 1000)
  end

  defp access_key(params, last_request_time, timeout_time) do
    case API.erlang_post_request(nil, "web_auth/access_key", params) do
      {:ok, {_code, %{"write_key" => write_key, "read_key" => read_key}, _headers}} ->
        %{write_key: write_key, read_key: read_key}

      {:ok, {_code, %{"message" => "request to be verified"}, _headers}} ->
        diff = System.os_time(:millisecond) - last_request_time

        if diff < 1000 do
          Process.sleep(1000 - diff)
        end

        access_key(params, System.os_time(:millisecond), timeout_time)
    end
  end

  defp access_key(_, last_request_time, timeout_time) when timeout_time > last_request_time do
    raise "Browser-based authentication has timed out"
  end
end
