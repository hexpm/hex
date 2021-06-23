defmodule Mix.Tasks.Hex.Retire do
  use Mix.Task

  @shortdoc "Retires a package version"

  @moduledoc """
  Retires a package version.

      mix hex.retire PACKAGE VERSION REASON

      mix hex.retire PACKAGE VERSION --unretire

  Mark a package as retired when you no longer recommend it's usage. A retired
  package is still resolvable and usable but it will be flagged as retired in the
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

    * `--message "MESSAGE"` - Required message (up to 140 characters) clarifying
      the retirement reason
    * `--organization ORGANIZATION` - Set this for private packages belonging to an organization
  
  ## Sample usage
    * `mix hex.retire your_app 0.1.1 invalid --message "Package has a breaking bug"`
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [message: :string, unretire: :boolean, organization: :string]

  @impl true
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
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.retire PACKAGE VERSION REASON
        mix hex.retire PACKAGE VERSION --unretire
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"PACKAGE VERSION REASON", "Retires a package version"},
      {"PACKAGE VERSION --unretire", "Unretires a package"}
    ]
  end

  defp retire(organization, package, version, reason, opts) do
    auth = Mix.Tasks.Hex.auth_info(:write)
    body = %{reason: reason, message: message_option(opts[:message])}

    case Hex.API.Release.retire(organization, package, version, body, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        Hex.Shell.info("#{package} #{version} has been retired\n")

        Hex.Shell.warn(
          "Retiring a version does not affect if the version will still be resolved. " <>
            "We recommend that you publish a new version of this package, unless there is " <>
            "already a more recent patch version of this package, because this version may " <>
            "still be picked by dependency resolution."
        )

        :ok

      other ->
        Hex.Shell.error("Retiring package failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp unretire(organization, package, version) do
    auth = Mix.Tasks.Hex.auth_info(:write)

    case Hex.API.Release.unretire(organization, package, version, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        Hex.Shell.info("#{package} #{version} has been unretired")
        :ok

      other ->
        Hex.Shell.error("Unretiring package failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp message_option(nil) do
    Mix.raise("Missing required flag --message")
  end

  defp message_option(message) do
    message
  end
end
