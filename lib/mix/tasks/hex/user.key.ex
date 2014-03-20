defmodule Mix.Tasks.Hex.User.Key do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Generate new API key"

  @moduledoc """
  Generate new API key and store on local machine.

  `mix hex.user.key -u username -p password`

  ## Command line options

  * `--user`, `-u` - Username of user (required)

  * `--pass`, `-p` - Password of user (required)
  """

  @aliases [u: :user, p: :pass]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :pass])

    Hex.start_api
    Util.generate_key(opts[:user], opts[:pass])
  end
end
