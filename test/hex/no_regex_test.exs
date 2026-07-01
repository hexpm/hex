defmodule Hex.NoRegexTest do
  use ExUnit.Case, async: true

  # Compiled regexes are embedded into beam files, and the compiled PCRE
  # pattern format is not portable across OTP versions (for example,
  # :re.import/1 does not exist before OTP 28.1). The Hex archive is
  # precompiled once and installed on many OTP versions, so code under
  # lib/ must not use regexes; parse with pattern matching instead.
  test "lib/ contains no regexes" do
    paths = Path.wildcard("lib/**/*.ex")
    assert paths != []

    offenders =
      Enum.filter(paths, fn path ->
        content = File.read!(path)
        String.contains?(content, ["~r", "~R", "Regex.", ":re."])
      end)

    assert offenders == [],
           "Regex usage found in shipped code: #{inspect(offenders)}. " <>
             "Regexes must not appear under lib/, see comment in #{__ENV__.file}"
  end
end
