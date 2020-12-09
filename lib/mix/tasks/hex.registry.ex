defmodule Mix.Tasks.Hex.Registry do
  use Mix.Task
  @behaviour Hex.Mix.TaskDescription

  @switches [
    name: :string,
    public_dir: :string,
    private_key: :string
  ]

  @shortdoc "Manages local Hex registries"

  @moduledoc """
  Manages local Hex registries.

  ## Build a local registry

  To build a registry you need a name, a directory that will be used to store public registry files,
  and a private key to sign the registry:

  ```
  $ mix hex.registry build --name=acme --public-dir=public --private-key=private_key.pem
  * creating public/public_key
  * creating public/tarballs
  * creating public/names
  * creating public/versions
  ```

  You can generate a random private key using the following command:

  ```
  $ openssl genrsa -out private_key.pem
  ```

  Let's say you have a package `foo-1.0.0.tar`. To publish it, simply copy it to the appropriate
  directory and re-build the registry:

  ```
  $ cp foo-1.0.0.tar public/tarballs/
  $ mix hex.registry build --name=acme --public-dir=public --private-key=private_key.pem
  * using private_key.pem
  * creating public/packages/foo
  * updating public/names
  * updating public/versions
  ```

  You can test the repository by starting the built-in Erlang/OTP HTTP server, adding the repository,
  and retrieving the package that you just published.

  ```
  $ erl -s inets -eval 'inets:start(httpd,[{port,8000},{server_name,"localhost"},{server_root,"."},{document_root,"public"}]).'

  # replace "acme" with the name of your repository
  $ mix hex.repo add acme http://localhost:8000 --public-key=public/public_key
  $ mix hex.package fetch foo 1.0.0 --repo=acme
  ```

  ### Command line options

    * `--name` - The name of the registry

    * `--public-dir` - Path to the directory with public files

    * `--private-key` - Path to the private key
  """
  @impl true
  def run(args) do
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      ["build"] ->
        build(opts)

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.registry build
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"build", "Build a local registry"}
    ]
  end

  defp build(opts) do
    repo_name = opts[:name] || raise "missing --name"
    public_dir = opts[:public_dir] || raise "missing --public-dir"
    private_key_path = opts[:private_key] || raise "missing --private-key"
    private_key = private_key_path |> File.read!() |> decode_private_key()
    build(repo_name, public_dir, private_key)
  end

  defp build(repo_name, public_dir, private_key) do
    ensure_public_key(private_key, public_dir)
    create_directory(Path.join(public_dir, "tarballs"))

    paths_per_name =
      Enum.group_by(Path.wildcard("#{public_dir}/tarballs/*.tar"), fn path ->
        [name, _version] = String.split(Path.basename(path), ["-", ".tar"], trim: true)
        name
      end)

    versions =
      Enum.map(paths_per_name, fn {name, paths} ->
        releases =
          paths
          |> Enum.map(&build_release(repo_name, &1))
          |> Enum.sort(&(Hex.Version.compare(&1.version, &2.version) == :lt))

        package =
          %{repository: repo_name, name: name, releases: releases}
          |> :mix_hex_registry.encode_package()
          |> sign_and_gzip(private_key)

        write_file("#{public_dir}/packages/#{name}", package)
        {name, Enum.map(releases, & &1.version)}
      end)

    for path <- Path.wildcard("#{public_dir}/packages/*"),
        not (Path.basename(path) in Map.keys(paths_per_name)) do
      remove_file(path)
    end

    names = for {name, _} <- versions, do: %{name: name}
    payload = %{repository: repo_name, packages: names}
    names = payload |> :mix_hex_registry.encode_names() |> sign_and_gzip(private_key)
    write_file("#{public_dir}/names", names)

    versions = for {name, versions} <- versions, do: %{name: name, versions: versions}
    payload = %{repository: repo_name, packages: versions}
    versions = payload |> :mix_hex_registry.encode_versions() |> sign_and_gzip(private_key)
    write_file("#{public_dir}/versions", versions)
  end

  defp build_release(repo_name, tarball_path) do
    tarball = File.read!(tarball_path)
    {:ok, result} = :mix_hex_tarball.unpack(tarball, :memory)

    dependencies =
      for {package, map} <- Map.get(result.metadata, "requirements", []) do
        %{
          "app" => app,
          "optional" => optional,
          "requirement" => requirement,
          "repository" => repository
        } = map

        release = %{
          package: package,
          app: app,
          optional: optional,
          requirement: requirement
        }

        if repository == repo_name do
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

  defp sign_and_gzip(protobuf, private_key) do
    protobuf
    |> :mix_hex_registry.sign_protobuf(private_key)
    |> :zlib.gzip()
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
