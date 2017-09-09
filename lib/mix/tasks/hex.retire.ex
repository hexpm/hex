defmodule Mix.Tasks.Hex.Retire do
  use Mix.Task

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
    * `--organization ORGANIZATION` - The organization the package belongs to
  """

  @switches [message: :string, unretire: :boolean, organization: :string]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    retire? = !opts[:unretire]
    organization = opts[:organization]

    case args do
      [package, version, reason] when retire? ->
        retire(organization, package, version, reason, opts)
      [package, version] when not retire? ->
        unretire(organization, package, version)
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:

        mix hex.retire PACKAGE VERSION REASON
        mix hex.retire PACKAGE VERSION --unretire
        """
    end
  end

  defp retire(organization, package, version, reason, opts) do
    auth = Mix.Tasks.Hex.auth_info()
    body = %{reason: reason, message: opts[:message]}

    case Hex.API.Release.retire(organization, package, version, body, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error "Retiring package failed"
        Hex.Utils.print_error_result(other)
    end
  end

  defp unretire(organization, package, version) do
    auth = Mix.Tasks.Hex.auth_info()

    case Hex.API.Release.unretire(organization, package, version, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error "Unretiring package failed"
        Hex.Utils.print_error_result(other)
    end
  end
end
