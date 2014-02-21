defmodule Mix.Tasks.Hex.Release do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @aliases [u: :user, p: :password]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)
    Util.required_opts(opts, [:user, :password])

    Mix.Task.run "compile"
    Mix.Project.get!
    config = Mix.project

    Hex.start_api
    if create_package?(config, opts) do
      create_release(config, opts)
    end
  end

  defp create_package?(config, opts) do
    meta = config[:package] || []

    case Hex.API.new_package(config[:app], meta, opts) do
      { code, _ } when code in [200, 201] ->
        true

      { code, body } ->
        Mix.shell.error("Updating package #{config[:app]} failed! (#{code})")
        Util.print_error_result(body)
        false
    end
  end

  defp create_release(config, opts) do
    reqs = Hex.Mix.deps_to_requirements(config[:deps] || [])
    git_url = config[:source_url]
    git_ref = git_ref()

    unless git_url do
      raise Mix.Error, message: "Missing source_url, specifying the git repo url, option in mix.exs"
    end

    case Hex.API.new_release(config[:app], config[:version], git_url, git_ref, reqs, opts) do
      { 201, _ } ->
        Mix.shell.info("Updating package #{config[:app]} and creating " <>
          "release #{config[:version]} was successful!")
      { code, body } ->
        Mix.shell.error("Creating release #{config[:app]} #{config[:version]} failed! (#{code})")
        Util.print_error_result(body)
    end
  end

  @ref_regex ~r/^
      [0-9A-Fa-f]+
      $/x

  defp git_ref do
    ref = System.cmd("git rev-parse --verify --quiet HEAD")
          |> String.strip

    unless Regex.match?(@ref_regex, ref) do
      raise Mix.Error, message: "Invalid git ref, are you in a git repository?"
    end

    ref
  end
end
