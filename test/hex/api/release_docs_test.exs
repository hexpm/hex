defmodule Hex.API.ReleaseDocsTest do
  use HexTest.Case

  test "uploads the body once when OTP is required" do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    tarball = "docs tarball"
    test_pid = self()
    send(test_pid, {:mix_shell_input, :prompt, "123456"})

    Bypass.expect(bypass, fn conn ->
      assert conn.request_path == "/api/packages/example/releases/1.0.0/docs"
      assert Plug.Conn.get_req_header(conn, "expect") == ["100-continue"]
      assert Plug.Conn.get_req_header(conn, "content-length") == ["12"]

      case Plug.Conn.get_req_header(conn, "x-hex-otp") do
        [] ->
          conn
          |> Plug.Conn.put_resp_header(
            "www-authenticate",
            ~s(Bearer realm="hex", error="totp_required")
          )
          |> Plug.Conn.resp(401, "")

        ["123456"] ->
          conn = Plug.Conn.inform(conn, 100, [])
          {:ok, body, conn} = Plug.Conn.read_body(conn)
          send(test_pid, {:body, body})
          Plug.Conn.resp(conn, 201, "")
      end
    end)

    progress = fn size -> send(test_pid, {:progress, size}) end

    assert {:ok, {201, _headers, nil}} =
             Hex.API.ReleaseDocs.publish(
               nil,
               "example",
               "1.0.0",
               tarball,
               [key: "token"],
               progress
             )

    assert_received {:mix_shell, :prompt, ["Enter OTP code:"]}
    assert_received {:body, ^tarball}
    assert_received {:progress, 12}
    refute_received {:progress, _size}
  end
end
