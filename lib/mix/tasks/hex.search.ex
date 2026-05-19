defmodule Mix.Tasks.Hex.Search do
  use Mix.Task

  @shortdoc "Open and perform documentation search"
  @search_url "https://search.hexdocs.pm/"
  @stdlib_apps [:elixir, :iex, :logger, :ex_unit, :eex, :mix]

  @moduledoc """
  Open and perform searches.

  When invoked without arguments, it opens up a search page on
  https://hexdocs.pm with all of your dependencies plus Elixir standard
  library selected:

      $ mix hex.search
      $ mix hex.search --no-stdlib
      $ mix hex.search --packages foo,bar

  You may also pass command line flags, to execute searches
  via the command line, according to the modes below.

  For searching package names, see `mix hex.package search`.

  ## Docs search

  Pass a single positional argument to search docs and print results
  directly to the terminal.

      $ mix hex.search "ecto changeset"
      $ mix hex.search "ecto changeset" --packages ecto,ecto_sql
      $ mix hex.search "ecto changeset" --limit 25

  ### Options

    * `--packages PACKAGE1,PACKAGE2,...` - Restrict docs search to an explicit package list
    * `--limit NUMBER` - Limit terminal docs search results (default: 10)
    * `--print-url` - Print the docs URL instead of opening it in the browser

  """
  @behaviour Hex.Mix.TaskDescription

  @switches [
    limit: :integer,
    packages: :string,
    stdlib: :boolean,
    print_url: :boolean
  ]

  @impl true
  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches)

    case args do
      [] -> hexdocs_search(opts)
      [query] -> docs_query_search(query, opts)
      _ -> invalid_args!()
    end
  end

  @impl true
  def tasks() do
    [
      {"", "Opens up hexdocs.pm with your dependencies"},
      {"--packages PACKAGE1,PACKAGE2,...", "Opens up hexdocs.pm with explicit packages"},
      {"QUERY", "Searches docs and prints results"},
      {"QUERY --packages PACKAGE1,PACKAGE2,...", "Searches docs within explicit packages"},
      {"QUERY --limit NUMBER", "Searches docs with a custom result limit"},
      {"QUERY --no-stdlib", "Searches docs without stdlib packages"}
    ]
  end

  defp hexdocs_search(opts) do
    if opts[:limit] do
      invalid_args!()
    end

    # Hexdocs accepts package1,package2 and
    # it will automatically fetch the latest
    packages =
      case opts[:packages] do
        nil -> project_packages(opts)
        packages -> parse_packages!(packages)
      end
      |> Enum.join(",")
      |> URI.encode_www_form()

    url = "https://hexdocs.pm/?packages=#{packages}&q="

    if opts[:print_url] do
      Hex.Shell.info(url)
    else
      Hex.Utils.system_open(url)
    end
  end

  defp docs_query_search(query, opts) do
    if opts[:print_url] do
      invalid_args!()
    end

    validate_limit!(opts[:limit])

    # Typesense expects exact project-vsn pairs
    packages =
      case opts[:packages] do
        nil ->
          opts
          |> project_packages()
          |> Enum.map(&String.replace(&1, ":", "-"))

        packages ->
          packages
          |> parse_packages!()
          |> Enum.map(&add_version_to_package!/1)
      end

    url =
      @search_url <>
        "?" <>
        URI.encode_query(
          %{q: query, query_by: "doc,title", filter_by: "package:=[#{Enum.join(packages, ",")}]"}
          |> maybe_put_limit(opts[:limit])
        )

    case search_http_module().request(:get, url, %{}, nil, %{}) do
      {:ok, {200, _headers, body}} ->
        body
        |> decode_json!()
        |> print_results()

      {:ok, {status, _headers, body}} ->
        Mix.raise("Docs search failed with HTTP status #{status}: #{inspect(body)}")

      {:error, reason} ->
        Mix.raise("Docs search request failed: #{inspect(reason)}")
    end
  end

  defp latest_stable(releases) do
    %{"version" => version} =
      Enum.find(
        releases,
        %{"version" => nil},
        &(Version.parse!(&1["version"]).pre == [])
      )

    version
  end

  defp project_packages(opts) do
    Mix.Tasks.Deps.Loadpaths.run(["--no-compile", "--no-listeners"])
    Hex.start()

    deps =
      for {_app, info} <- Mix.Dep.Lock.read(),
          %{repo: "hexpm", name: name, version: version} <- [Hex.Utils.lock(info)] do
        "#{name}:#{version}"
      end

    stdlib =
      if Keyword.get(opts, :stdlib, true) do
        Enum.map(@stdlib_apps, &"#{&1}:#{System.version()}")
      else
        []
      end

    Enum.sort(deps ++ stdlib)
  end

  defp parse_packages!(packages) do
    packages =
      packages
      |> String.split(",", trim: true)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))

    if packages == [] do
      Mix.raise("Expected --packages to include at least one package name")
    end

    packages
  end

  defp add_version_to_package!(package)
       when package in ["elixir", "iex", "logger", "ex_unit", "eex", "mix"] do
    "#{package}-#{System.version()}"
  end

  defp add_version_to_package!(package) do
    Hex.start()

    case Hex.API.Package.get("hexpm", package) do
      {:ok, {200, _headers, %{"releases" => releases}}} ->
        case latest_stable(releases) do
          nil -> Mix.raise("Package #{package} has no stable release for docs search")
          version -> "#{package}-#{version}"
        end

      {:ok, {status, _headers, body}} ->
        Mix.raise(
          "Failed to resolve package #{package} for docs search, got HTTP status #{status}: #{inspect(body)}"
        )

      {:error, reason} ->
        Mix.raise("Failed to resolve package #{package} for docs search: #{inspect(reason)}")
    end
  end

  defp decode_json!(body) do
    if Code.ensure_loaded?(:json) do
      try do
        :json.decode(body)
      rescue
        error in ErlangError ->
          Mix.raise("Docs search returned invalid JSON: #{inspect(error.original)}")
      end
    else
      Mix.raise(":json module is not available, upgrade OTP to use this feature")
    end
  end

  defp print_results(%{"found" => 0}) do
    Hex.Shell.info("No results found")
  end

  defp print_results(%{"found" => _found, "hits" => hits}) do
    total = Integer.to_string(length(hits))

    hits
    |> Enum.with_index(1)
    |> Enum.each(fn {hit, index} ->
      %{
        "document" => %{
          "doc" => doc,
          "package" => package,
          "ref" => ref,
          "title" => title
        }
      } = hit

      Hex.Shell.info([
        :bright,
        ["# ", title, " (", Integer.to_string(index), "/", total, ")"],
        :reset,
        ["\n", document_url(package, ref), "\n\n"],
        [strip_html_comments(doc), "\n\n"]
      ])
    end)
  end

  defp document_url(package, ref) do
    case :binary.split(package, "-") do
      [name, version] ->
        "https://hexdocs.pm/#{Enum.join([name, version, ref], "/")}"

      _ ->
        Mix.raise("Unexpected package search result format: #{inspect(package)}")
    end
  end

  defp strip_html_comments(doc) do
    case :binary.split(doc, "<!--") do
      [before] ->
        before

      [before, rest] ->
        case :binary.split(rest, "-->") do
          [_comment] -> before
          [_comment, after_comment] -> before <> strip_html_comments(after_comment)
        end
    end
  end

  defp maybe_put_limit(params, nil), do: params
  defp maybe_put_limit(params, limit), do: Map.put(params, :per_page, limit)

  defp validate_limit!(nil), do: :ok
  defp validate_limit!(limit) when is_integer(limit) and limit > 0 and limit <= 250, do: :ok

  defp validate_limit!(limit) do
    Mix.raise("Expected --limit to be between 1 and 250, got: #{inspect(limit)}")
  end

  defp search_http_module do
    Application.get_env(:hex, :search_http_module, Hex.HTTP)
  end

  defp invalid_args! do
    Mix.raise("""
    Invalid arguments, expected:

    mix hex.search
    mix hex.search --packages PACKAGE1,PACKAGE2,...
    mix hex.search QUERY
    mix hex.search QUERY --packages PACKAGE1,PACKAGE2,...
    mix hex.search QUERY --limit NUMBER
    """)
  end
end
