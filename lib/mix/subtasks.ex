defmodule Mix.Subtasks do
  @type args :: String.t()
  @type docs :: String.t()
  @type subtask_spec :: {args, docs}
  @callback subtasks() :: [subtask_spec]
end
