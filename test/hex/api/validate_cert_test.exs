defmodule Hex.API.ValidateCertTest do
  use ExUnit.Case

  setup_all do
    {:ok, apps} = :application.ensure_all_started(:ssl)

    on_exit fn ->
      for app <- apps, do: :application.stop(app)
    end




  end

  setup do

    :meck.new(Hex.API.Certs)

    :meck.expect(Hex.API.Certs, :cacerts, fn() -> [der_encoded_ca_cert] end)

    on_exit fn ->
      :meck.unload
    end    
  end

  def partial_chain_fun do
    opts = Hex.API.ssl_opts("https://foo.com")

    {_, partial_chain} = :lists.keyfind(:partial_chain, 1, opts)

    partial_chain
  end

  def der_encoded_ca_cert do
    {:ok, ca_cert} = :file.read_file("test/ca.cert.der")
    ca_cert
  end

  def server_cert do
    {:ok, server_cert} = :file.read_file("test/server.der")
    server_cert
  end

  def server_sub_cert do
    {:ok, server_sub_cert} = :file.read_file("test/server_sub.der")
    server_sub_cert
  end

  def untrusted_cert do
    {:ok, untrusted_cert} = :file.read_file("test/untrusted.der")
    untrusted_cert
  end



  def run_validation(chain, partial_chain) do
    :erlang.put(:ssl_manager, :ssl_manager.manager_name(:normal))

    if :erlang.function_exported(:ssl_manager, :connection_init, 2) do
      {:ok, cert_db_ref, cert_db_handle, _, _, _} = :ssl_manager.connection_init("test/ca.cert.pem", :client)
    else
      {:ok, cert_db_ref, cert_db_handle, _, _, _, _} = :ssl_manager.connection_init("test/ca.cert.pem", :client, {:ssl_crl_cache, {:internal, []}})
    end
      


    validation_fun_and_state = {fn(otp_cert, extension_or_verify_result, ssl_state) ->
                         if :erlang.function_exported(:ssl_certificate, :validate_extension, 3) do
                          :ssl_certificate.validate_extension(otp_cert,
                                                            extension_or_verify_result, ssl_state)
                         else
                            :ssl_certificate.validate(otp_cert,
                                                            extension_or_verify_result, ssl_state)
                         end                         
                 end, :client}

    {trusted_cert, cert_path}  =
            :ssl_certificate.trusted_cert_and_path(chain, cert_db_handle, cert_db_ref, partial_chain)

    :public_key.pkix_path_validation(trusted_cert,
                                              cert_path,
                                             [{:max_path_length,
                                               20},
                                              {:verify_fun, validation_fun_and_state}])  
  end


  if Hex.API.is_secure_ssl do
    test "succeeds to validate normal chain" do
      

      result = run_validation([server_cert], :undefined)

      assert {:ok, _} = result

    end

    test "fails to validate invalid chain" do


      result = run_validation([untrusted_cert], :undefined)

      assert {:error, {:bad_cert, :unknown_ca}} = result
    end

    test "fails to validate because CA bit is missing" do


      result= run_validation([server_sub_cert,server_cert], :undefined)
      assert {:error, {:bad_cert, :invalid_key_usage}} = result
    end

    test "fails to validate without partial chain" do


      result= run_validation([server_sub_cert,server_cert,untrusted_cert], :undefined)
      assert {:error, {:bad_cert, :unknown_ca}} = result
    end



    test "fails to validate with partial chain" do

      result= run_validation([server_sub_cert,server_cert,untrusted_cert], partial_chain_fun)
      assert {:error, {:bad_cert, :unknown_ca }} = result

    end


    def amazon_cert do
      {:ok, amazon_cert} = :file.read_file("test/amazon.der")
      amazon_cert    
    end

    def verisigng5 do
      {:ok, verisigng5} = :file.read_file("test/verisigng5.der")
      verisigng5
    end

    def verisign_g3 do
      {:ok, verisign_g3} = :file.read_file("test/verisign_g3.der")
      verisign_g3
    end



    test "fails to validate a partial chain without the partial chain option" do
      :meck.unload
      result= run_validation([amazon_cert,verisign_g3,verisigng5], :undefined)
      assert {:error, {:bad_cert, :unknown_ca}} = result

    end

    test "succeeds to validate with partial chain that is correct" do
      :meck.unload
      result= run_validation([amazon_cert,verisign_g3,verisigng5], partial_chain_fun)
      assert {:ok, _} = result

    end
  end


end
