defmodule Mix.Tasks.Hex.Registry do
  use Mix.Task
  @behaviour Hex.Mix.TaskDescription

  @switches [
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

  ### Command line options

    * `--name` - The name of the registry

    * `--private-key` - Path to the private key

  ## Add a package

      $ mix hex.registry add PUBLIC_DIR PACKAGE

  To add a package to an existing registry, supply the public directory of the registry and path to
  the new package. This action also requires the name of the registry and the private key originally
  used to generate it:

      $ mix hex.registry add public --name=acme --private-key=private_key.pem foo-1.0.0.tar
      * copying foo-1.0.0.tar -> public/tarballs/foo-1.0.0.tar
      * updating public/packages/foo
      * updating public/names
      * updating public/versions

  """
  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = OptionParser.parse!(args, strict: @switches)

    case args do
      ["add", public_dir, package] ->
        add(public_dir, package, opts)

      ["build", public_dir] ->
        build(public_dir, opts)

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.registry add PUBLIC_DIR PACKAGE
        mix hex.registry build PUBLIC_DIR
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"add PUBLIC_DIR PACKAGE", "Add package to a local registry"},
      {"build PUBLIC_DIR", "Build a local registry"}
    ]
  end

  ## Add

  defp add(public_dir, package, opts) do
    repo_name = opts[:name] || Mix.raise("missing --name")
    private_key_path = opts[:private_key] || Mix.raise("missing --private-key")
    private_key = private_key_path |> File.read!() |> decode_private_key()
    add(repo_name, public_dir, private_key, package)
  end

  defp add(repo_name, public_dir, private_key, package) do
    public_key = ensure_public_key(private_key, public_dir)
    tarball_dir = Path.join(public_dir, "tarballs")
    create_directory(tarball_dir)

    path = copy_file(package, tarball_dir)
    [package_name | _rest] = String.split(Path.basename(path), ["-", ".tar"], trim: true)

    existing_package_releases = read_package(repo_name, public_dir, public_key, package_name)
    new_package_release = build_release(repo_name, package)

    package_releases =
      [new_package_release | existing_package_releases]
      |> Enum.sort(&(Hex.Version.compare(&1.version, &2.version) == :lt))
      |> Enum.uniq_by(& &1.version)

    package =
      :mix_hex_registry.build_package(
        %{repository: repo_name, name: package_name, releases: package_releases},
        private_key
      )

    write_file("#{public_dir}/packages/#{package_name}", package)

    existing_names =
      read_names(repo_name, public_dir, public_key)
      |> Enum.map(fn %{name: name, updated_at: updated_at} -> {name, updated_at} end)
      |> Enum.into(%{})

    previous_updated_at = get_in(existing_names, [package_name, :updated_at, :seconds])
    new_version_updated_at = File.stat!(path).mtime
    updated_at = %{seconds: max_updated_at(previous_updated_at, new_version_updated_at), nanos: 0}
    package_versions = Enum.map(package_releases, & &1.version)

    repo_versions =
      read_versions(repo_name, public_dir, public_key)
      |> Enum.map(fn %{name: name, versions: versions} ->
        {name, %{updated_at: existing_names[name], versions: versions}}
      end)
      |> Enum.into(%{})
      |> Map.put(package_name, %{updated_at: updated_at, versions: package_versions})

    repo_names =
      Enum.map(repo_versions, fn {name, %{updated_at: updated_at}} ->
        %{name: name, updated_at: updated_at}
      end)

    payload = %{repository: repo_name, packages: repo_names}
    repo_names = :mix_hex_registry.build_names(payload, private_key)
    write_file("#{public_dir}/names", repo_names)

    repo_versions =
      Enum.map(repo_versions, fn {name, %{versions: versions}} ->
        %{name: name, versions: versions}
      end)

    payload = %{repository: repo_name, packages: repo_versions}
    repo_versions = :mix_hex_registry.build_versions(payload, private_key)
    write_file("#{public_dir}/versions", repo_versions)
  end

  ## Build

  defp build(public_dir, opts) do
    repo_name = opts[:name] || raise "missing --name"
    private_key_path = opts[:private_key] || raise "missing --private-key"
    private_key = private_key_path |> File.read!() |> decode_private_key()
    build(repo_name, public_dir, private_key)
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
          |> Enum.sort(&(Version.compare(&1.version, &2.version) == :lt))

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

  ## Registry utilities

  @unix_epoch :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})

  @doc false
  def to_unix(erl_datetime) do
    :calendar.datetime_to_gregorian_seconds(erl_datetime) - @unix_epoch
  end

  defp max_updated_at(previous_as_unix_or_nil, nil), do: previous_as_unix_or_nil
  defp max_updated_at(nil, current_as_datetime), do: to_unix(current_as_datetime)

  defp max_updated_at(previous_as_unix, current_as_datetime) do
    max(previous_as_unix, to_unix(current_as_datetime))
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

    encoded_public_key
  end

  defp read_names(repo_name, public_dir, public_key) do
    path = Path.join(public_dir, "names")
    payload = File.read!(path)
    repo_name_or_no_verify = repo_name || :no_verify

    case :mix_hex_registry.unpack_names(payload, repo_name_or_no_verify, public_key) do
      {:ok, names} ->
        names

      {:error, reason} ->
        Mix.raise(
          "Invalid package name manifest at #{path}: #{inspect(reason)}." <>
            "\n\nIs the repository name #{repo_name} correct?"
        )
    end
  end

  defp read_versions(repo_name, public_dir, public_key) do
    path = Path.join(public_dir, "versions")
    payload = File.read!(path)
    repo_name_or_no_verify = repo_name || :no_verify

    case :mix_hex_registry.unpack_versions(payload, repo_name_or_no_verify, public_key) do
      {:ok, versions} ->
        versions

      {:error, reason} ->
        Mix.raise(
          "Invalid package version manifest at #{path}: #{inspect(reason)}." <>
            "\n\nIs the repository name #{repo_name} correct?"
        )
    end
  end

  defp read_package(repo_name, public_dir, public_key, package_name) do
    path = Path.join([public_dir, "packages", package_name])

    case File.read(path) do
      {:ok, payload} ->
        case :mix_hex_registry.unpack_package(payload, repo_name, package_name, public_key) do
          {:ok, package} -> package
          _ -> []
        end

      _ ->
        []
    end
  end

  ## File utilities

  defp create_directory(path) do
    unless File.dir?(path) do
      Hex.Shell.info(["* creating ", path])
      File.mkdir_p!(path)
    end
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

  defp copy_file(path, destination_dir) do
    file = Path.basename(path)
    destination_file = Path.join(destination_dir, file)
    Hex.Shell.info(["* copying ", path, " -> ", destination_file])
    File.cp!(path, destination_file)
    destination_file
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
