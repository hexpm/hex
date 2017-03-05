defmodule Hex.HTTP.VerifyHostnameTest do
  use ExUnit.Case, async: true

  alias Hex.HTTP.VerifyHostname

  defmacrop assert_match(i, r, v) do
    quote do
      assert unquote(v) == VerifyHostname.validate_and_parse_wildcard_identifier(unquote(i), unquote(r))
      assert VerifyHostname.try_match_hostname(unquote(i), unquote(r))
    end
  end

  defmacrop refute_match(i, r) do
    quote do
      refute VerifyHostname.try_match_hostname(unquote(i), unquote(r))
    end
  end

  test "success" do
    assert_match('www.example.com',   'WWW.eXamPle.CoM',      false)
    assert_match('www.example.com.',  'www.example.com',      false)
    assert_match('www.example.com',   'www.example.com.',     false)
    assert_match('*.example.com',     'www.example.com',      {[], '.example.com', true})
    assert_match('b*z.example.com',   'buzz.example.com',     {'b', 'z.example.com', false})
    assert_match('*baz.example.com',  'foobaz.example.com',   {[], 'baz.example.com', false})
    assert_match('baz*.example.com',  'baz1.example.com',     {'baz', '.example.com', false})
  end

  test "failure" do
    refute_match('*.com',                   'eXamPle.CoM')
    refute_match('.com.',                   'example.com.')
    refute_match('*.www.example.com',       'www.example.com.')
    refute_match('foo.*.example.com',       'foo.bar.example.com.')
    refute_match('xn--*.example.com',       'xn-foobar.example.com')
    refute_match('*fooxn--bar.example.com', 'bazfooxn--bar.example.com')
    refute_match('*.akamaized.net',         'tv.eurosport.com')
    refute_match('a*c.example.com',         'abcd.example.com')
    refute_match('*baz.example.com',        'foobuzz.example.com')
  end
end
