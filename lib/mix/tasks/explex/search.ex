defmodule Mix.Tasks.Explex.Search do
  use Mix.Task
  alias Mix.Tasks.Explex.Util

  @aliases [u: :user, e: :email, p: :password]

  def run(args) do
    { _opts, args, _ } = OptionParser.parse(args)
    Explex.start_api

    case args do
      [package] ->
        case Explex.API.get_packages(package) do
          { 200, body } ->
            print_packages(body)
          { code, body } ->
            Mix.shell.error("Package search failed! (#{code})")
            Util.print_error_result(body)
        end
      _ ->
        raise Mix.Error, message: "invalid arguments, expected 'mix explex.search PACKAGE'"
    end
  end

  defp print_packages(body) do
    Enum.each(body, &Mix.shell.info(&1["name"]))
  end
end
