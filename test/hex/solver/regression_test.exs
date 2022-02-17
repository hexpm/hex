defmodule Hex.Solver.RegressionTest do
  use HexTest.Case
  import HexTest.SolverHelper
  alias Hex.Registry.Server, as: Registry

  # Older OTP versions cannot read ETS files created on OTP 23
  if :erlang.system_info(:otp_release) < '19' do
    @moduletag :skip
  end

  setup do
    Hex.State.put(:offline, true)
  end

  defp open_registry(filename) do
    Registry.open(registry_path: HexTest.Case.fixture_path(Path.join("registries", filename)))
  end

  # How to create snapshot of registry:
  # Hex.Dev.extract_registry(top_level_packages, "test/fixtures/registries/CURRENT_DATE.ets")

  # https://elixirforum.com/t/mix-deps-update-working-as-mix-deps-downgrade/27099/10
  test "20200917" do
    open_registry("20200917.ets")

    assert solve(jason: "~> 1.0", postgrex: ">= 0.0.0", phoenix_params: "~> 1.1") == %{
             connection: "1.0.4",
             db_connection: "2.2.2",
             decimal: "1.9.0",
             jason: "1.2.2",
             mime: "1.4.0",
             phoenix: "1.5.4",
             phoenix_params: "1.1.3",
             phoenix_pubsub: "2.0.0",
             plug: "1.10.4",
             plug_crypto: "1.1.2",
             postgrex: "0.15.5",
             telemetry: "0.4.2"
           }
  end

  # https://github.com/hexpm/hex/issues/901
  test "issue/901" do
    open_registry("20210915.ets")

    result = %{
      cowboy: "2.9.0",
      cowboy_telemetry: "0.3.1",
      cowlib: "2.11.0",
      mime: "2.0.1",
      phoenix: "1.5.12",
      phoenix_pubsub: "2.0.0",
      plug: "1.12.1",
      plug_cowboy: "2.5.2",
      plug_crypto: "1.2.2",
      ranch: "1.8.0",
      telemetry: "0.4.3"
    }

    assert solve(phoenix: "~> 1.5", plug_cowboy: "~> 2.0") == result
    # the order of the deps should not affect the result
    assert solve(plug_cowboy: "~> 2.0", phoenix: "~> 1.5") == result
  end

  test "20210926" do
    open_registry("20210926.ets")

    assert solve(chromic_pdf: "~> 1.1", telemetry: "~> 0.4 or ~> 1.0", telemetry_poller: "~> 0.5") ==
             %{
               chromic_pdf: "1.1.1",
               jason: "1.2.2",
               nimble_pool: "0.2.4",
               telemetry: "0.4.3",
               telemetry_poller: "0.5.1"
             }
  end

  test "issue #571" do
    # earmark and ex_doc at 2018-06-09
    setup_registry(Path.join(test_tmp(), "cache.ets"), [
      {:hexpm, :earmark,
       ~w(0.1.0 0.1.1 0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7 0.1.8 0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.2.0 0.2.1 1.0.0 1.0.1 1.0.2 1.0.3 1.1.0 1.1.1 1.2.0 1.2.1 1.2.2 1.2.3 1.2.4 1.2.5),
       []},
      {:hexpm, :ex_doc, ~w(0.5.1 0.5.2 0.6.0 0.6.1 0.6.2 0.7.0 0.7.1 0.7.2), []},
      {:hexpm, :ex_doc,
       ~w(0.7.3 0.8.0 0.8.1 0.8.2 0.8.3 0.8.4 0.9.0 0.10.0 0.11.0 0.11.1 0.11.2 0.11.3 0.11.4 0.11.5),
       [{:earmark, "~> 0.1.17 or ~> 0.2", true}]},
      {:hexpm, :ex_doc, ~w(0.12.0), [earmark: "~> 0.2"]},
      {:hexpm, :ex_doc, ~w(0.13.0 0.13.1 0.13.2 0.14.0 0.14.1 0.14.2 0.14.3 0.14.4 0.14.5),
       [earmark: "~> 1.0"]},
      {:hexpm, :ex_doc,
       ~w(0.15.0 0.15.1 0.16.0 0.16.1 0.16.2 0.16.3 0.16.4 0.17.0 0.17.1 0.17.2 0.18.0 0.18.1 0.18.2 0.18.3),
       [earmark: "~> 1.1"]}
    ])

    assert solve(earmark: "~> 0.1", ex_doc: "~> 0.11") == %{earmark: "0.2.1", ex_doc: "0.12.0"}

    assert solve([earmark: "~> 0.1", ex_doc: "~> 0.11"], earmark: "0.2.1") == %{
             earmark: "0.2.1",
             ex_doc: "0.12.0"
           }

    assert solve([earmark: "~> 0.1", ex_doc: "~> 0.11"], ex_doc: "0.12.0") == %{
             earmark: "0.2.1",
             ex_doc: "0.12.0"
           }
  end
end
