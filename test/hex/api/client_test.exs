defmodule Hex.API.ClientTest do
  use HexTest.Case

  alias Hex.API.Client

  describe "config/1 OTP handling" do
    test "uses the OTP passed in opts" do
      assert Client.config(otp: "123456").api_otp == "123456"
    end

    test "falls back to HEX_OTP (api_otp state) for non-interactive use" do
      Hex.State.put(:api_otp, "654321")

      assert Client.config().api_otp == "654321"
    end

    test "prefers an explicit opts OTP over the api_otp state" do
      Hex.State.put(:api_otp, "654321")

      assert Client.config(otp: "123456").api_otp == "123456"
    end

    test "leaves api_otp undefined when no OTP is available" do
      Hex.State.put(:api_otp, nil)

      assert Client.config().api_otp == :undefined
    end
  end
end
