ExUnit.start

defmodule TestHelper do
  defmacro in_tmp(fun) do
    path = Path.join([tmp_path, __CALLER__.module, elem(__CALLER__.function, 0)])
    quote do
      path = unquote(path)
      File.rm_rf!(path)
      File.mkdir_p!(path)
      File.cd!(path, fn -> unquote(fun).(path) end)
    end
  end

  def tmp_path do
    Path.expand("../tmp", __DIR__)
  end
end
