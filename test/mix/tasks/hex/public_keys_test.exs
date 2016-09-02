defmodule Mix.Tasks.Hex.PublicKeysTest do
  use HexTest.Case
  @moduletag :integration

  @private_key """
  -----BEGIN RSA PRIVATE KEY-----
  MIIEogIBAAKCAQEAv2vdOJMC5+xG/4LW64Rc0Fr79Sp3bteQvJpzxASIrGB+gV43
  PCWDo6M/zIQbwCixZrXKhKN4MIDpmrYJcjDZqagf/KN4nCV9+5aGGXAoYIU22lF/
  S1AjAJVSxPiwjLZeuL2IPizkujKlZosyj1DVBjdysQNIGxYrWJzchWJXCTqKXqyh
  22FvWbduMIlBQlgz1oYWSTGol4j3MxOn4JzhRe/8udCysE5ureSUMRmVYGLTreXa
  FE73XptGuLI+9cwN3TMU8/qpTRJiMed/l1d5pntLNWUauJ2Ifq8/EAfTEGyTvNwq
  acrPNqgyCXMoMD53vUwdbd0F5vOdLwHmLka57wIDAQABAoIBAAkVb0wN/edPmRbq
  +JP4lgb1Klu6BizmYeHxWmkE5g94DMjK1cgu545OYuqroj1+MNz054Vf1IZIt801
  skEO/GVGqy3r+/888DuOKxLt9ZyO9clqcKX8SYzviRv7yTUEOn590AdTTg9f8+Vn
  2aBfb5x56SFpTHSsmqX726MgxYyUe2VTK0ljRRcv2MJq1Ce+LD0x0Hdo0MR2Otyq
  6ltZllWjQCfz2p/rinRJBXb1phrtj2WdDoRxtuqP7v7hA5IuXh+/vX5xZvcv+Fyc
  gTLIkckyRAeIyOCRWWLxla9ltcgCOoWmq5ZMbaWsgNc4boeSFas9850Sv9WBZBr/
  z8BNVKECgYEA/TMoR/K9g2a1tHdfOwKJJswOEWvNAf9jNdHJ9bjWbLZP3UGQYS74
  9mnmzOPzGMrJlwnEElGBQGhkL8sQflmNyhS95mEsoi/oUS3kjIBclscIXPdbf/am
  mae0Dle1EENNnhyU2xnLIXWtOHYpKEn+jqZfDF9T4t2LAjC2Ybdf59cCgYEAwYnN
  udejOUmwkTPY+q2XFAHnVkGMJb+TnpG1HXjxSP9xjK9my1mk9JNi4c7zsWIYzG5s
  10b7iNewSza6FKNTzmRfrXz/9Z1VkOKvVUSaCWpkgcR5MJ/eE9uCyzoaB/gWryyc
  KDcjaG+kAf6F8hH9oIsFhq7bDEv7pvquEbU1G6kCgYA2Pjdq8n3XILHcr5YfNjPb
  s+tvoACTZPsEFKWHlsmZEJWbeOhNaZq8b4OacVsq/IajbNwscHmYKsUL2Bz4dIcD
  u93S9Q5y0ICco5BK6VTsmEcaY4OermCSmnLgf7myejiunsDlD2mNCYDKj8XRc8it
  FsdWBzYZbAzTs0vfM0HAkQKBgB0V0ZYBQyVj3qYjrZMWrWKzWonK9f3iSZQF/7r7
  Jv+6a90hqwgaY8DhuXWH/XWF8YYffjnoD/sjtvnGsrZRSVH2Ia7X3zuRNn+8oW7m
  DWQm2g3qcfANxnkjfwd9ptXVwcr3oEwm5SpXxMUL9CNJ8tzCp8Ty2DaD4MDaYXiF
  zAJpAoGAN7Xa4KNg+FU0D3MEa9bB1UiEZeF4Lm+vqtBGERmwzatN7UryQnEnm0C9
  vVIpYWoOX3VrCKDoXyrLpcYYY5csbqad9zjnz5D9KGlJEKgn2nZGy/WbBHVO/W/H
  0DGNVv/jZuq4z3O81SL8t+AdUPUC4ZA031WchBonEBx8bEIwKTg=
  -----END RSA PRIVATE KEY-----
  """

  @public_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAv2vdOJMC5+xG/4LW64Rc
  0Fr79Sp3bteQvJpzxASIrGB+gV43PCWDo6M/zIQbwCixZrXKhKN4MIDpmrYJcjDZ
  qagf/KN4nCV9+5aGGXAoYIU22lF/S1AjAJVSxPiwjLZeuL2IPizkujKlZosyj1DV
  BjdysQNIGxYrWJzchWJXCTqKXqyh22FvWbduMIlBQlgz1oYWSTGol4j3MxOn4Jzh
  Re/8udCysE5ureSUMRmVYGLTreXaFE73XptGuLI+9cwN3TMU8/qpTRJiMed/l1d5
  pntLNWUauJ2Ifq8/EAfTEGyTvNwqacrPNqgyCXMoMD53vUwdbd0F5vOdLwHmLka5
  7wIDAQAB
  -----END PUBLIC KEY-----
  """

  test "list default keys" do
    Mix.Tasks.Hex.PublicKeys.run(["list"])
    assert_received {:mix_shell, :info, ["* https://repo.hex.pm"]}
  end

  test "add and remove keys" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      File.write!("my_key.pem", @public_key)
      Mix.Tasks.Hex.PublicKeys.run(["add", "http://other.repo", "my_key.pem", "--force"])

      verify!()

      Mix.Tasks.Hex.PublicKeys.run(["list"])
      assert_received {:mix_shell, :info, ["* https://repo.hex.pm"]}
      assert_received {:mix_shell, :info, ["* http://other.repo"]}

      Mix.Tasks.Hex.PublicKeys.run(["remove", "http://other.repo"])
      Mix.Tasks.Hex.PublicKeys.run(["list"])
      assert_received {:mix_shell, :info, ["* https://repo.hex.pm"]}
      refute_received {:mix_shell, :info, ["* http://other.repo"]}
    end
  end

  test "fails to verify with wrong key" do
    Hex.State.put(:hexpm_pk, @public_key)
    assert_raise Mix.Error, fn ->
      verify!()
    end
  end

  test "local keys override in-memory" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      File.write!("my_key.pem", @public_key)
      Mix.Tasks.Hex.PublicKeys.run(["add", "https://repo.hex.pm", "my_key.pem", "--force"])

      Mix.Tasks.Hex.PublicKeys.run(["list"])
      assert_received {:mix_shell, :info, ["* https://repo.hex.pm"]}

      assert_raise Mix.Error, fn ->
        verify!()
      end
    end
  end

  test "fails when no public key is stored" do
    Hex.State.put(:repo, Hex.State.fetch!(:mirror))
    assert_raise Mix.Error, fn ->
      verify!()
    end
  end

  test "fetch signature from file" do
    bypass_registry()
    repo = Hex.State.fetch!(:repo)

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      File.write!("my_key.pem", @public_key)
      Mix.Tasks.Hex.PublicKeys.run(["add", repo, "my_key.pem", "--force"])
      verify!()
    end
  end

  test "fails to verify signature from file" do
    bypass_registry()
    repo = Hex.State.fetch!(:repo)

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      File.write!("my_key.pem", Hex.State.fetch!(:hexpm_pk))
      Mix.Tasks.Hex.PublicKeys.run(["add", repo, "my_key.pem", "--force"])
      assert_raise Mix.Error, fn ->
        verify!()
      end
    end
  end

  defp bypass_registry do
    bypass = bypass_setup()

    Bypass.expect bypass, fn
      %Plug.Conn{request_path: "/packages/postgrex"} = conn ->
        file =
          %{payload: "foobar", signature: sign("foobar")}
          |> :hex_pb_signed.encode_msg(:Signed)
          |> :zlib.gzip
        Plug.Conn.resp(conn, 200, file)
    end

    bypass
  end

  defp bypass_setup do
    bypass = Bypass.open
    Hex.State.put(:repo, "http://localhost:#{bypass.port}")
    bypass
  end

  defp sign(file) do
    [entry | _ ] = :public_key.pem_decode(@private_key)
    key = :public_key.pem_entry_decode(entry)

    :public_key.sign(file, :sha512, key)
  end

  defp verify! do
    {200, body, _headers} = Hex.API.Registry.get_package("postgrex")
    body
    |> :zlib.gunzip
    |> Hex.API.Registry.verify
  end
end
