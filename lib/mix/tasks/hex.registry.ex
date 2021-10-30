defmodule Mix.Tasks.Hex.Registry do
  use Mix.Task
  @behaviour Hex.Mix.TaskDescription

  @switches [
    incremental: :boolean,
    name: :string,
    private_key: :string
  ]

  @shortdoc "Manages local Hex registries"

  @moduledoc """
  Manages local Hex registries.

  ## Build a local registry

      $ mix hex.registry build PUBLIC_DIR

  To build a registry you need a name, a directory that will be used to store public registry files,
  and a private key to sign the registry:

      $ mix hex.registry build public --name=acme --private-key=private_key.pem
      * creating public/public_key
      * creating public/tarballs
      * creating public/names
      * creating public/versions

  You can generate a random private key using the following command:

      $ openssl genrsa -out private_key.pem

  Let's say you have a package `foo-1.0.0.tar`. To publish it, simply copy it to the appropriate
  directory and re-build the registry:

      $ cp foo-1.0.0.tar public/tarballs/
      $ mix hex.registry build public --name=acme --private-key=private_key.pem
      * creating public/packages/foo
      * updating public/names
      * updating public/versions

  You can test the repository by starting the built-in Erlang/OTP HTTP server, adding the repository,
  and retrieving the package that you just published.

      $ erl -s inets -eval 'inets:start(httpd,[{port,8000},{server_name,"localhost"},{server_root,"."},{document_root,"public"}]).'

      # replace "acme" with the name of your repository
      $ mix hex.repo add acme http://localhost:8000 --public-key=public/public_key
      $ mix hex.package fetch foo 1.0.0 --repo=acme

  To use the package in your Mix project, add it as a dependency and set the `:repo` option to your repository name:

      defp deps() do
        {:decimal, "~> 2.0", repo: "acme"}
      end

  ### Incremental Builds

  By default, `mix hex.registry build` will create a registry that includes packages and versions
  present in the tarball directory. This means that missing tarballs will remove the corresponding
  versions or packages from the registry.

  You can optionally perform an incremental build of the registry using the `--incremental`
  command line option. This will add artifacts in the tarball directory without removing any of
  the other versions or packages. This may be useful, for example, in a CI environment in which
  you would like to publish to a local registry without downloading tarballs.

  To successfully run an incremental build, the following files are still required:

      * PUBLIC_DIR/names
      * PUBLIC_DIR/versions

  as well as

      * PUBLIC_DIR/packages/PACKAGE_NAME

  for any existing packages to which you intend to add an additional version.

  ### Command line options

    * `--name` - The name of the registry

    * `--private-key` - Path to the private key

    * `--incremental` - Use incremental registry building (see Incremental Builds)

  """
  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      ["build", public_dir] ->
        build(public_dir, opts)

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.registry build PUBLIC_DIR
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"build PUBLIC_DIR", "Build a local registry"}
    ]
  end

  defp build(public_dir, opts) do
    repo_name = opts[:name] || raise "missing --name"
    private_key_path = opts[:private_key] || raise "missing --private-key"
    private_key = private_key_path |> File.read!() |> decode_private_key()

    if opts[:incremental] do
      build_incremental(repo_name, public_dir, private_key)
    else
      build(repo_name, public_dir, private_key)
    end
  end

  defp build(repo_name, public_dir, private_key) do
    ensure_public_key(private_key, public_dir)
    create_directory(Path.join(public_dir, "tarballs"))

    paths_per_name =
      Enum.group_by(Path.wildcard("#{public_dir}/tarballs/*.tar"), fn path ->
        [name | _rest] = String.split(Path.basename(path), ["-", ".tar"], trim: true)
        name
      end)

    versions =
      Enum.map(paths_per_name, fn {name, paths} ->
        releases =
          paths
          |> Enum.map(&build_release(repo_name, &1))
          |> Enum.sort(&(Hex.Version.compare(&1.version, &2.version) == :lt))

        updated_at =
          paths
          |> Enum.map(&File.stat!(&1).mtime)
          |> Enum.sort()
          |> Enum.at(-1)

        updated_at = updated_at && %{seconds: to_unix(updated_at), nanos: 0}

        package =
          :mix_hex_registry.build_package(
            %{repository: repo_name, name: name, releases: releases},
            private_key
          )

        write_file("#{public_dir}/packages/#{name}", package)
        versions = Enum.map(releases, & &1.version)
        {name, %{updated_at: updated_at, versions: versions}}
      end)

    for path <- Path.wildcard("#{public_dir}/packages/*"),
        not Enum.member?(Map.keys(paths_per_name), Path.basename(path)) do
      remove_file(path)
    end

    names =
      for {name, %{updated_at: updated_at}} <- versions do
        %{name: name, updated_at: updated_at}
      end

    payload = %{repository: repo_name, packages: names}
    names = :mix_hex_registry.build_names(payload, private_key)
    write_file("#{public_dir}/names", names)

    versions =
      for {name, %{versions: versions}} <- versions do
        %{name: name, versions: versions}
      end

    payload = %{repository: repo_name, packages: versions}
    versions = :mix_hex_registry.build_versions(payload, private_key)
    write_file("#{public_dir}/versions", versions)
  end

  defp build_incremental(repo_name, public_dir, private_key) do
    ensure_public_key(private_key, public_dir)
    public_key = read_file!(Path.join(public_dir, "public_key"))

    existing_names =
      read_names!(repo_name, public_dir, public_key)
      |> Enum.map(fn %{name: name, updated_at: updated_at} -> {name, updated_at} end)
      |> Enum.into(%{})

    existing_versions =
      read_versions!(repo_name, public_dir, public_key)
      |> Enum.map(fn %{name: name, versions: versions} ->
        {name, %{updated_at: existing_names[name], versions: versions}}
      end)
      |> Enum.into(%{})

    create_directory(Path.join(public_dir, "tarballs"))

    paths_per_name =
      Enum.group_by(Path.wildcard("#{public_dir}/tarballs/*.tar"), fn path ->
        [name | _rest] = String.split(Path.basename(path), ["-", ".tar"], trim: true)
        name
      end)

    versions =
      Enum.map(paths_per_name, fn {name, paths} ->
        existing_releases = read_package!(repo_name, public_dir, public_key, name)

        releases =
          paths
          |> Enum.map(&build_release(repo_name, &1))
          |> Enum.concat(existing_releases)
          |> Enum.sort(&(Hex.Version.compare(&1.version, &2.version) == :lt))
          |> Enum.uniq_by(& &1.version)

        updated_at =
          paths
          |> Enum.map(&File.stat!(&1).mtime)
          |> Enum.sort()
          |> Enum.at(-1)

        existing_updated_at = get_in(existing_names, [name, :updated_at, :seconds])

        max_updated_at =
          cond do
            is_nil(updated_at) ->
              existing_updated_at

            is_nil(existing_updated_at) ->
              to_unix(updated_at)

            true ->
              max(to_unix(updated_at), existing_updated_at)
          end

        max_updated_at = %{seconds: max_updated_at, nanos: 0}

        package =
          :mix_hex_registry.build_package(
            %{repository: repo_name, name: name, releases: releases},
            private_key
          )

        write_file("#{public_dir}/packages/#{name}", package)
        versions = Enum.map(releases, & &1.version)
        {name, %{updated_at: max_updated_at, versions: versions}}
      end)
      |> Enum.into(%{})

    combined_versions = Map.merge(existing_versions, versions)

    names =
      for {name, %{updated_at: updated_at}} <- combined_versions do
        %{name: name, updated_at: updated_at}
      end

    payload = %{repository: repo_name, packages: names}
    names = :mix_hex_registry.build_names(payload, private_key)
    write_file("#{public_dir}/names", names)

    combined_versions =
      for {name, %{versions: versions}} <- combined_versions do
        %{name: name, versions: versions}
      end

    payload = %{repository: repo_name, packages: combined_versions}
    combined_versions = :mix_hex_registry.build_versions(payload, private_key)
    write_file("#{public_dir}/versions", combined_versions)
  end

  ## Build utilities

  @unix_epoch :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})

  @doc false
  def to_unix(erl_datetime) do
    :calendar.datetime_to_gregorian_seconds(erl_datetime) - @unix_epoch
  end

  defp build_release(repo_name, tarball_path) do
    tarball = File.read!(tarball_path)
    {:ok, result} = :mix_hex_tarball.unpack(tarball, :memory)

    dependencies =
      for {package, map} <- Map.get(result.metadata, "requirements", []) do
        app = Map.fetch!(map, "app")
        requirement = Map.fetch!(map, "requirement")
        optional = map["optional"] == true
        repository = map["repository"]

        release = %{
          package: package,
          app: app,
          optional: optional,
          requirement: requirement
        }

        if !repository or repository == repo_name do
          release
        else
          Map.put(release, :repository, repository)
        end
      end

    %{
      version: result.metadata["version"],
      inner_checksum: result.inner_checksum,
      outer_checksum: result.outer_checksum,
      dependencies: dependencies
    }
  end

  defp ensure_public_key(private_key, public_dir) do
    path = "#{public_dir}/public_key"
    encoded_public_key = private_key |> extract_public_key() |> encode_public_key()

    case File.read(path) do
      {:ok, ^encoded_public_key} ->
        :ok

      {:ok, _} ->
        Hex.Shell.info("* public key at #{path} does not match private key, overwriting")
        write_file(path, encoded_public_key)

      {:error, :enoent} ->
        write_file(path, encoded_public_key)
    end
  end

  ## Incremental build utilities

  defp read_names!(repo_name, public_dir, public_key) do
    path = Path.join(public_dir, "names")
    payload = read_file!(path)

    case :mix_hex_registry.unpack_names(payload, repo_name, public_key) do
      {:ok, names} ->
        names

      _ ->
        Mix.raise("""
        Invalid package name manifest at #{path}

        Is the repository name #{repo_name} correct?
        """)
    end
  end

  defp read_versions!(repo_name, public_dir, public_key) do
    path = Path.join(public_dir, "versions")
    payload = read_file!(path)

    case :mix_hex_registry.unpack_versions(payload, repo_name, public_key) do
      {:ok, versions} ->
        versions

      _ ->
        Mix.raise("""
        Invalid package version manifest at #{path}

        Is the repository name #{repo_name} correct?
        """)
    end
  end

  defp read_package!(repo_name, public_dir, public_key, package_name) do
    path = Path.join([public_dir, "packages", package_name])

    with {:ok, payload} <- read_file(path),
         {:ok, package} <-
           :mix_hex_registry.unpack_package(payload, repo_name, package_name, public_key) do
      package
    else
      _ -> []
    end
  end

  ## File utilities

  defp create_directory(path) do
    unless File.dir?(path) do
      Hex.Shell.info(["* creating ", path])
      File.mkdir_p!(path)
    end
  end

  defp read_file!(path) do
    if File.exists?(path) do
      Hex.Shell.info(["* reading ", path])
    else
      Mix.raise("""
      Error reading file #{path}

      Using --incremental requires an existing registry
      """)
    end

    File.read!(path)
  end

  defp read_file(path) do
    if File.exists?(path) do
      Hex.Shell.info(["* reading ", path])
    else
      Hex.Shell.info(["* skipping ", path])
    end

    File.read(path)
  end

  defp write_file(path, data) do
    if File.exists?(path) do
      Hex.Shell.info(["* updating ", path])
    else
      File.mkdir_p!(Path.dirname(path))
      Hex.Shell.info(["* creating ", path])
    end

    File.write!(path, data)
  end

  defp remove_file(path) do
    Hex.Shell.info(["* removing ", path])
    File.rm!(path)
  end

  ## Key utilities

  require Record

  Record.defrecordp(
    :rsa_private_key,
    :RSAPrivateKey,
    Record.extract(:RSAPrivateKey, from_lib: "public_key/include/OTP-PUB-KEY.hrl")
  )

  Record.defrecordp(
    :rsa_public_key,
    :RSAPublicKey,
    Record.extract(:RSAPublicKey, from_lib: "public_key/include/OTP-PUB-KEY.hrl")
  )

  defp extract_public_key(rsa_private_key(modulus: m, publicExponent: e)) do
    rsa_public_key(modulus: m, publicExponent: e)
  end

  defp encode_public_key(key) do
    :public_key.pem_encode([:public_key.pem_entry_encode(:RSAPublicKey, key)])
  end

  defp decode_private_key(data) do
    [entry] = :public_key.pem_decode(data)
    :public_key.pem_entry_decode(entry)
  end
end
