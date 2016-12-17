defmodule Mix.Tasks.Hex.Retire do
  use Mix.Task
  alias Mix.Hex.Utils

  @shortdoc "Retires a package version"

  @moduledoc """
  Retires a package version.

      mix hex.retire PACKAGE VERSION REASON

      mix hex.retire PACKAGE VERSION --unretire

  Mark a package as retired when you no longer recommend it's usage. A retired
  is still resolvable and usable but it will be flagged as retired in the
  repository and a message will be displayed to users when they use the package.

  ## Retirement reasons

    * **renamed** - The package has been renamed, including the new package name
      in the message
    * **deprecated** - The package has been deprecated, if there's a replacing
      package include it in the message
    * **security** - There are security issues with this package
    * **invalid** - The package is invalid, for example it does not compile correctly
    * **other** - Any other reason not included above, clarify the reason in
      the message

  ## Command line options

    * `--message "MESSAGE"` - Optional message (up to 140 characters) clarifying
      the retirement reason
  """

  @switches [message: :string, unretire: :boolean]

  def run(args) do
    Hex.start

    {opts, args, _} = OptionParser.parse(args, switches: @switches)
    config = Hex.Config.read
    retire? = !opts[:unretire]

    case args do
      [package, version, reason] when retire? ->
        auth = Utils.auth_info(config)
        retire(package, version, reason, opts, auth)
      [package, version] when not retire? ->
        auth = Utils.auth_info(config)
        unretire(package, version, auth)
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.retire PACKAGE VERSION REASON
        mix hex.retire PACKAGE VERSION --unretire
        """
    end
  end

  defp retire(package, version, reason, opts, auth) do
    body = %{reason: reason, message: opts[:message]}
    case Hex.API.Release.retire(package, version, body, auth) do
      {code, _body, _headers} when code in 200..299 ->
        :ok
      {code, body, _headers} ->
        Hex.Shell.error "Retiring package failed"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp unretire(package, version, auth) do
  case Hex.API.Release.unretire(package, version, auth) do
    {code, _body, _headers} when code in 200..299 ->
      :ok
    {code, body, _headers} ->
      Hex.Shell.error "Unretiring package failed"
      Hex.Utils.print_error_result(code, body)
  end
  end
end
