defmodule Mix.Tasks.Hex.Register do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Register a new hex user"

  @moduledoc """
  Registers a new hex user.

  `mix hex.register -u username -e email -p password`

  ## Command line options

  * `--user`, `-u` - Username for the user, used for authentication (required)

  * `--email`, `-e` - Email of the user, used for contact information if there
    are issues with a package. Will not be shared (required)

  * `--password`, `-p` - Password for the user, used for authentication
    (required)
  """

  @aliases [u: :user, e: :email, p: :password]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :email, :password])

    Hex.start_api
    create_user(opts)
  end

  defp create_user(opts) do
    case Hex.API.new_user(opts[:user], opts[:email], opts[:password]) do
      { 201, _ } ->
        Mix.shell.info("Registration of user #{opts[:user]} successful!")
      { code, body } ->
        Mix.shell.error("Registration of user #{opts[:user]} failed! (#{code})")
        Util.print_error_result(code, body)
    end
  end
end
