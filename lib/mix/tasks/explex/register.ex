defmodule Mix.Tasks.Explex.Register do
  use Mix.Task
  alias Mix.Tasks.Explex.Util

  @aliases [u: :user, e: :email, p: :password]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :email, :password])

    Explex.start_api
    create_user(opts)
  end

  defp create_user(opts) do
    case Explex.API.new_user(opts[:user], opts[:email], opts[:password]) do
      { 201, _ } ->
        Mix.shell.info("Registration of user #{opts[:user]} successful!")
      { code, body } ->
        Mix.shell.error("Registration of user #{opts[:user]} failed! (#{code})")
        Util.print_error_result(body)
    end
  end
end
