defmodule Hex.ParallelTest do
  use HexTest.Case

  test "runs at most max_jobs jobs at a time" do
    name = :hex_parallel_test
    start_supervised!({Hex.Parallel, [name, fn -> 2 end]})
    parent = self()

    for id <- 1..3 do
      Hex.Parallel.run(name, id, fn ->
        send(parent, {:started, id, self()})

        receive do
          :finish -> id
        end
      end)
    end

    assert_receive {:started, 1, job1}
    assert_receive {:started, 2, job2}
    refute_receive {:started, 3, _}, 100

    send(job1, :finish)
    assert Hex.Parallel.await(name, 1, 1_000) == 1
    assert_receive {:started, 3, job3}

    send(job2, :finish)
    send(job3, :finish)
    assert Hex.Parallel.await(name, 2, 1_000) == 2
    assert Hex.Parallel.await(name, 3, 1_000) == 3
  end

  test "fetches registry files with four times the tarball concurrency" do
    concurrency = Hex.State.fetch!(:http_concurrency)

    assert :sys.get_state(:hex_tarball_fetcher).max_jobs == concurrency
    assert :sys.get_state(:hex_registry_fetcher).max_jobs == 4 * concurrency
  end
end
