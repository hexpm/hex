defmodule Hex.Mix.TaskDescription do
  @moduledoc false

  @type args :: String.t()
  @type docs :: String.t()
  @type task_spec :: {args, docs}
  @callback tasks() :: [task_spec]
end
