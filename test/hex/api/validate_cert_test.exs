defmodule Hex.API.ValidateCertTest do
  use ExUnit.Case, async: true

  @moduletag :skip

  setup_all do
    {:ok, _} = Application.ensure_all_started(:ssl)
  end

  def partial_chain_fun do
    &Hex.API.SSL.partial_chain([der_encoded_ca_cert()], &1)
  end

  def cert_path(name) do
    Path.join(["test", "fixtures", "certs", name])
  end

  def read_cert(name) do
    cert_path(name)
    |> File.read!
  end

  def der_encoded_ca_cert,
    do: read_cert("ca.cert.der")

  def server_cert,
    do: read_cert("server.der")

  def server_sub_cert,
    do: read_cert("server_sub.der")

  def untrusted_cert,
    do: read_cert("untrusted.der")

  def amazon_cert,
    do: read_cert("amazon.der")

  def verisigng5,
    do: read_cert("verisigng5.der")

  def verisign_g3,
    do: read_cert("verisign_g3.der")

  def run_validation(chain, partial_chain) do
    Process.put(:ssl_manager, :ssl_manager.manager_name(:normal))

    [:ok, cert_db_ref, cert_db_handle | _] =
      if function_exported?(:ssl_manager, :connection_init, 2) do
        :ssl_manager.connection_init(cert_path("ca.cert.pem"), :client)
      else
        :ssl_manager.connection_init(cert_path("ca.cert.pem"), :client, {:ssl_crl_cache, {:internal, []}})
      end
      |> Tuple.to_list

    verify_fun =
      if function_exported?(:ssl_certificate, :validate_extension, 3) do
        {&:ssl_certificate.validate_extension/3, :client}
      else
        {&:ssl_certificate.validate/3, :client}
      end

    {trusted_cert, cert_path} =
      :ssl_certificate.trusted_cert_and_path(chain, cert_db_handle, cert_db_ref, partial_chain)

    :public_key.pkix_path_validation(trusted_cert, cert_path, max_path_length: 20, verify_fun: verify_fun)
  end

  if Hex.API.SSL.secure_ssl? do
    test "succeeds to validate normal chain" do
      assert {:ok, _} = run_validation([server_cert()], :undefined)
    end

    test "fails to validate invalid chain" do
      assert {:error, {:bad_cert, :unknown_ca}} = run_validation([untrusted_cert()], :undefined)
    end

    test "fails to validate because CA bit is missing" do
      assert {:error, {:bad_cert, :invalid_key_usage}} = run_validation([server_sub_cert(), server_cert()], :undefined)
    end

    test "fails to validate without partial chain" do
      assert {:error, {:bad_cert, :unknown_ca}} = run_validation([server_sub_cert(), server_cert(), untrusted_cert()], :undefined)
    end

    test "fails to validate with partial chain" do
      assert {:error, {:bad_cert, :unknown_ca}} = run_validation([server_sub_cert(), server_cert(), untrusted_cert()], partial_chain_fun())
    end

    test "fails to validate a partial chain without the partial chain option" do
      assert {:error, {:bad_cert, :unknown_ca}} = run_validation([amazon_cert(), verisign_g3(), verisigng5()], :undefined)
    end

    test "succeeds to validate with partial chain that is correct" do
      partial_chain_fun = &Hex.API.SSL.partial_chain(Hex.API.Certs.cacerts, &1)
      assert {:ok, _} = run_validation([amazon_cert(), verisign_g3(), verisigng5()], partial_chain_fun)
    end
  end
end
