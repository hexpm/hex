defmodule Hex.Resolver.RegressionTest do
  use HexTest.Case
  import HexTest.ResolverHelper
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

    deps = [jason: "~> 1.0", postgrex: ">= 0.0.0", phoenix_params: "~> 1.1"]

    result =
      locked(
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
      )

    assert equal?(result, resolve(deps))
  end

  # https://github.com/hexpm/hex/issues/901
  test "issue/901" do
    open_registry("20210915.ets")

    deps = [phoenix: "~> 1.5", plug_cowboy: "~> 2.0"]

    result =
      locked(
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
      )

    assert equal?(result, resolve(deps))
    # the order of the deps should not affect the result
    assert equal?(result, resolve(Enum.reverse(deps)))
  end
end
