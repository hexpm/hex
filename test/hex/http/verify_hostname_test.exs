defmodule Hex.HTTP.VerifyHostnameTest do
  use ExUnit.Case, async: true

  alias Hex.HTTP.VerifyHostname

  defmacrop assert_match(i, r, v) do
    quote do
      assert unquote(v) ==
               VerifyHostname.validate_and_parse_wildcard_identifier(unquote(i), unquote(r))

      assert VerifyHostname.try_match_hostname(unquote(i), unquote(r))
    end
  end

  defmacrop refute_match(i, r) do
    quote do
      refute VerifyHostname.try_match_hostname(unquote(i), unquote(r))
    end
  end

  test "success" do
    assert_match(~c"www.example.com", ~c"WWW.eXamPle.CoM", false)
    assert_match(~c"www.example.com.", ~c"www.example.com", false)
    assert_match(~c"www.example.com", ~c"www.example.com.", false)
    assert_match(~c"*.example.com", ~c"www.example.com", {[], ~c".example.com", true})
    assert_match(~c"b*z.example.com", ~c"buzz.example.com", {~c"b", ~c"z.example.com", false})
    assert_match(~c"*baz.example.com", ~c"foobaz.example.com", {[], ~c"baz.example.com", false})
    assert_match(~c"baz*.example.com", ~c"baz1.example.com", {~c"baz", ~c".example.com", false})
  end

  test "failure" do
    refute_match(~c"*.com", ~c"eXamPle.CoM")
    refute_match(~c".com.", ~c"example.com.")
    refute_match(~c"*.www.example.com", ~c"www.example.com.")
    refute_match(~c"foo.*.example.com", ~c"foo.bar.example.com.")
    refute_match(~c"xn--*.example.com", ~c"xn-foobar.example.com")
    refute_match(~c"*fooxn--bar.example.com", ~c"bazfooxn--bar.example.com")
    refute_match(~c"*.akamaized.net", ~c"tv.eurosport.com")
    refute_match(~c"a*c.example.com", ~c"abcd.example.com")
    refute_match(~c"*baz.example.com", ~c"foobuzz.example.com")
  end
end
