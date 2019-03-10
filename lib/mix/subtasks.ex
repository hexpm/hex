defmodule Hex.Mix.Subtasks do
  @moduledoc false

  @type args :: String.t()
  @type docs :: String.t()
  @type subtask_spec :: {args, docs}
  @callback subtasks() :: [subtask_spec]
end
