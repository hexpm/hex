defmodule Hex.OrganizationAuthIntegrationTest do
  use HexTest.IntegrationCase, async: false

  test "the changed server filters enforcing organizations on refresh and Hex retains 2FA reasons" do
    auth = Hexpm.new_oauth_user("tfa_client", "tfa_client@example.com", "hunter42")
    auth = Keyword.put(auth, :oauth, true)
    suffix = System.unique_integer([:positive])
    enforcing = "enforced_#{suffix}"
    unaffected = "unaffected_#{suffix}"
    assert {:ok, {204, _, _}} = Hexpm.new_repo(enforcing, auth)
    assert {:ok, {204, _, _}} = Hexpm.new_repo(unaffected, auth)
    config = Hex.API.Client.config(auth)

    assert {:ok, {204, _, _}} =
             :mix_hex_api.post(config, ["organization_tfa"], %{"organization" => enforcing})

    assert :ok = Hex.Auth.refresh_tokens(Hex.API.Client.config([]))

    assert Hex.OAuth.organization_reauth_required() == [
             %{organization: enforcing, requirements: ["tfa"]}
           ]

    assert {:ok, {201, _, response}} = Hex.API.OAuth.organization_authorization([enforcing])
    assert response["verification_uri"] =~ "/organizations/authorize?code="
    assert response["expires_in"] > 0
    token = Hex.State.get(:oauth_token).access_token
    [_header, payload, _signature] = String.split(token, ".")
    assert {:ok, decoded} = Base.url_decode64(payload, padding: false)
    assert is_binary(decoded)
    assert decoded =~ "repository:#{unaffected}"
    refute decoded =~ "repository:#{enforcing}"
    send(self(), {:mix_shell_input, :yes?, false})
    Hex.RemoteConverger.check_organization_reauth([enforcing, unaffected])
    assert_received {:mix_shell, :yes?, [question]}
    assert question =~ "#{enforcing}: 2FA verification required"
    refute question =~ "SSO authentication required"
  end

  test "an accepted API OTP restores organization access without another authentication prompt" do
    in_tmp("organization_tfa_write_client_review", fn ->
      set_home_cwd()
      suffix = System.unique_integer([:positive])
      username = "review_tfa_#{suffix}"
      organization = "review_tfa_org_#{suffix}"
      secret = "JBSWY3DPEHPK3PXP"
      config = Hex.API.Client.config()

      assert {:ok, {201, _, _}} =
               :mix_hex_api.post(config, ["user"], %{
                 "username" => username,
                 "email" => "#{username}@example.com",
                 "password" => "hunter42",
                 "tfa" => %{"secret" => secret}
               })

      assert {:ok, {200, _, response}} =
               :mix_hex_api.post(config, ["oauth_token"], %{
                 "username" => username,
                 "scope" => "api repositories"
               })

      Hex.OAuth.store_token(%{
        access_token: response["access_token"],
        refresh_token: response["refresh_token"],
        expires_at: System.system_time(:second) + response["expires_in"]
      })

      auth = [key: response["access_token"], oauth: true, otp: totp(secret)]
      assert {:ok, {204, _, _}} = Hexpm.new_repo(organization, auth)
      Hexpm.new_package(organization, "review_tfa_pkg", "1.0.0", [], %{}, auth)

      assert {:ok, {204, _, _}} =
               :mix_hex_api.post(Hex.API.Client.config(auth), ["organization_tfa"], %{
                 "organization" => organization
               })

      assert {:ok, {200, _, fresh_session}} =
               :mix_hex_api.post(config, ["oauth_token"], %{
                 "username" => username,
                 "scope" => "api repositories"
               })

      Hex.OAuth.store_token(%{
        access_token: fresh_session["access_token"],
        refresh_token: fresh_session["refresh_token"],
        expires_at: System.system_time(:second) + fresh_session["expires_in"]
      })

      assert :ok = Hex.Auth.refresh_tokens(Hex.API.Client.config())

      assert [%{organization: ^organization, requirements: ["tfa"]}] =
               Hex.OAuth.organization_reauth_required()

      Hex.State.put(:api_otp, nil)
      send(self(), {:mix_shell_input, :prompt, totp(secret)})
      Mix.Tasks.Hex.Owner.run(["add", "review_tfa_pkg", username, "--organization", organization])
      output = flush()
      assert Enum.any?(output, &match?({:mix_shell, :prompt, ["Enter OTP code:"]}, &1))
      refute Enum.any?(output, &match?({:mix_shell, :yes?, _}, &1))
      refute Enum.any?(output, &match?({:mix_shell, :error, ["Adding owner failed"]}, &1))
      assert :ok = Hex.Auth.refresh_tokens(Hex.API.Client.config())
      assert Hex.OAuth.organization_reauth_required() == []
      assert {:ok, {200, _, owners}} = Hex.API.Package.Owner.get(organization, "review_tfa_pkg")
      assert Enum.any?(owners, &(&1["username"] == username))
    end)
  end

  defp totp(secret) do
    counter = div(System.system_time(:second), 30) + 1
    digest = :crypto.mac(:hmac, :sha, Base.decode32!(secret), <<counter::64>>)
    offset = Bitwise.band(:binary.last(digest), 15)
    <<_::binary-size(offset), code::32, _::binary>> = digest

    code
    |> Bitwise.band(0x7FFFFFFF)
    |> rem(1_000_000)
    |> Integer.to_string()
    |> String.pad_leading(6, "0")
  end
end
