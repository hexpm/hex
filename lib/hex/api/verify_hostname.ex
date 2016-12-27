defmodule Hex.API.VerifyHostname do
  # Based on https://github.com/deadtrickster/ssl_verify_hostname.erl

  require Record

  Record.defrecordp :attribute_type_and_value, :AttributeTypeAndValue,
    Record.extract(:AttributeTypeAndValue, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :otp_tbs_certificate, :OTPTBSCertificate,
    Record.extract(:OTPTBSCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :otp_certificate, :OTPCertificate,
    Record.extract(:OTPCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :extension, :Extension,
    Record.extract(:Extension, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  @id_ce_subject_alt_name {2, 5, 29, 17}
  @id_at_common_name      {2, 5, 4, 3}


  def verify_cert_hostname(otp_certificate(tbsCertificate: tbs_cert), hostname) do
    dns_names = extract_dns_names(tbs_cert)
    dns_name_matched = try_match_hostnames(dns_names, hostname)

    if maybe_check_subject_cn(dns_names, dns_name_matched, tbs_cert, hostname) do
      {:valid, hostname}
    else
      {:fail, :unable_to_match_altnames}
    end
  end

  def verify_fun(_, {:bad_cert, _} = reason, _),
    do: {:fail, reason}

  def verify_fun(_, {:extension, _}, state),
    do: {:unknown, state}

  def verify_fun(_, :valid, state),
    do: {:valid, state}

  def verify_fun(cert, :valid_peer, state) do
    if check_hostname = state[:check_hostname] do
      verify_cert_hostname(cert, check_hostname)
    else
      {:valud, state}
    end
  end

  def validate_and_parse_wildcard_identifier(identifier, hostname) do
    wildcard_pos = :string.chr(identifier, ?*)
    valid? =
      wildcard_pos != 0 and
        length(hostname) >= length(identifier) and
        check_wildcard_in_leftmost_label(identifier, wildcard_pos)

    if valid? do
      before_w = :string.substr(identifier, 1, wildcard_pos - 1)
      after_w = :string.substr(identifier, wildcard_pos + 1)

      if :string.chr(after_w, ?*) == 0 do
        case check_two_labels_after_wildcard(after_w) do
          {:ok, dot_after_wildcard} ->
            single_char_w = dot_after_wildcard == wildcard_pos and length(before_w) == 0
            {before_w, after_w, single_char_w}
          :error ->
            false
        end
      else
        false
      end
    else
      false
    end
  end

  def try_match_hostname(identifier, hostname) do
    identifier = :string.strip(identifier, :right, ?.)
    hostname = :string.strip(hostname, :right, ?.)

    if case_insensitve_match(identifier, hostname) do
      true
    else
      case validate_and_parse_wildcard_identifier(identifier, hostname) do
        {before_w, after_w, single_char_w} ->
          try_match_wildcard(before_w, after_w, single_char_w, hostname)
        false ->
          false
      end
    end
  end

  defp extract_cn({:rdnSequence, list}),
    do: do_extract_cn(list)

  defp do_extract_cn([[attribute_type_and_value(type: @id_at_common_name, value: cn)]|_]),
    do: cn

  defp do_extract_cn([_|rest]),
    do: rest

  defp do_extract_cn([]),
    do: []

  defp extract_dns_names(otp_tbs_certificate(extensions: extensions)) do
    extensions = :pubkey_cert.extensions_list(extensions)
    alt_subject = :pubkey_cert.select_extension(@id_ce_subject_alt_name, extensions)

    if alt_subject == :undefined do
      []
    else
      extract_dns_names_from_alt_names(extension(alt_subject, :extnValue))
    end
  end

  defp extract_dns_names_from_alt_names(extn_values) do
    Enum.reduce(extn_values, [], fn extn_value, acc ->
      case extn_value do
        {:dNSName, dns_name} ->
          [dns_name|acc]
        _ ->
          acc
      end
    end)
  end

  defp case_insensitve_match(string1, string2),
    do: :string.to_lower(string1) == :string.to_lower(string2)

  defp wildcard_not_in_label(before_w, after_w) do
    dot_pos = :string.chr(after_w, ?.)
    after_dot = :string.substr(after_w, 1, dot_pos)
    :string.str(before_w, 'xn--') == 0 and :string.str(after_dot, 'xn--') == 0
  end

  defp try_match_wildcard(before_w, after_w, single_char_w, pattern) do
    dot_pos = :string.chr(pattern, ?.)
    if single_char_w do
      case_insensitve_match(after_w, :string.substr(pattern, dot_pos))
    else
      if wildcard_not_in_label(before_w, after_w) do
        pattern_part1 = :string.substr(pattern, length(pattern) - length(after_w) + 1, length(after_w))
        pattern_part2 = :string.substr(pattern, 1, length(before_w))
        case_insensitve_match(after_w, pattern_part1) and
        case_insensitve_match(before_w, pattern_part2)
      else
        false
      end
    end
  end

  defp check_two_labels_after_wildcard(string) do
    {_, positions} =
      Enum.reduce(string, {1, []}, fn
        ?., {ix, acc} -> {ix + 1, [ix|acc]}
        _,  {ix, acc} -> {ix + 1, acc}
      end)

    if length(positions) >= 2 do
      {:ok, List.last(positions)}
    else
      :error
    end
  end

  defp check_wildcard_in_leftmost_label(identifier, wildcard_pos) do
    dot_pos = :string.chr(identifier, ?.)
    dot_pos != 0 and dot_pos >= wildcard_pos
  end

  defp try_match_hostnames(dns_names, hostname),
    do: Enum.any?(dns_names, &try_match_hostname(&1, hostname))

  defp maybe_check_subject_cn(_dns_names, true, _tbs_cert, _hostname),
    do: true

  defp maybe_check_subject_cn([_|_], false, _tbs_cert, _hostname),
    do: false

  defp maybe_check_subject_cn(_dns_names, false, tbs_cert, hostname) do
    tbs_cert
    |> otp_tbs_certificate(:subject)
    |> extract_cn
    |> try_match_hostname(hostname)
  end
end
