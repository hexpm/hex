defmodule HexTest.IntegrationCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use HexTest.Case

      setup_all do
        Code.require_file("../setup_hexpm.exs", unquote(__DIR__))
        :ok
      end
    end
  end
end
