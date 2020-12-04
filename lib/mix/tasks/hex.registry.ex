# TODO:
#
#  * Should this be `mix hex.repo build` instead?
#
#  * The current approach is naive, we're always rebuilding all registry files.
#    
#    An alternative is to do smart partial updates by publishing tarballs one at a time
#    but then the usage is likely instead:
#
#       mix hex.registry init
#       mix hex.registry publish PATH_TO_TARBALL
#
#    Another advantage of this is we could implement retires:
#
#       mix hex.registry retire PACKAGE VERSION
#
#    Which we can't have in the naive approach. I'm not sure how important retirements
#    are in local registries though so that's why I started with something much simpler.
#
#  * We were planning to introduce `:hex_registry.build` to hex_core as a building block
#    but with the current approach I'm not sure it makes sense anymore.
#
#    If we extract the logic as is, it seems too local FS centric and thus wouldn't
#    be used by projects like Hex.pm (which want to save stuff to external storage
#    and do partial updates) so not appropriate for hex_core?
#
#    That being said, I'd be fine extracting it if only so rebar3 can easily use it.
#
#    What we for sure could extract though is slightly nicer way to build registry resources,
#    instead:
#
#        %{repository: repo_name, packages: packages}
#        |> :mix_hex_registry.encode_names()
#        |> :mix_hex_registry.sign_protobuf(private_key)
#        |> :zlib.gzip()
#
#    we'd have:
#
#       :mix_hex_registry.something_something_names(%{...}, private_key)
#
#  * Couple more TODOs are inline
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

      mix hex.registry build

  After running this task, the current working directory will look like this:

      public/
        tarballs/
        names
        versions
        public_key
      private_key.pem

  You can publish packages by copying the tarballs to `public/tarballs/` and re-running
  `mix hex.registry build`.

  ### Command line options

    * `--name` - The name of the registry, defaults to the basename of the current working
      directory

    * `--public-dir` - Path to the directory with registry public files, defaults to
      `./public`

    * `--private-key` - Path to the private key, defaults to `./private_key.pem`
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
    repo_name = opts[:name] || Path.dirname(File.cwd!())
    public_dir = opts[:public_dir] || "public"
    private_key_path = opts[:private_key] || "private_key.pem"
    private_key = get_private_key(private_key_path)
    ensure_public_key(private_key, public_dir)
    ensure_public_tarballs(public_dir)

    versions =
      Path.wildcard("#{public_dir}/tarballs/*.tar")
      |> Enum.group_by(fn path ->
        [name, _version] = String.split(Path.basename(path), ["-", ".tar"], trim: true)
        name
      end)
      |> Enum.map(fn {name, paths} ->
        releases =
          for path <- paths do
            tarball = File.read!(path)
            {:ok, result} = :mix_hex_tarball.unpack(tarball, :memory)

            # TOOD:
            # dependencies = Enum.map(result.metadata["requirements"], ...)
            dependencies = []

            %{
              version: result.metadata["version"],
              inner_checksum: result.inner_checksum,
              outer_checksum: result.outer_checksum,
              dependencies: dependencies
            }
          end
          |> Enum.sort(&(Hex.Version.compare(&1.version, &2.version) == :lt))

        package =
          %{repository: repo_name, name: name, releases: releases}
          |> :mix_hex_registry.encode_package()
          |> sign_and_gzip(private_key)

        write_file("#{public_dir}/packages/#{name}", package)
        {name, Enum.map(releases, & &1.version)}
      end)

    # TODO: remove public_dir/package/* that no longer have corresponding tarballs

    names = for {name, _} <- versions, do: %{name: name}
    payload = %{repository: repo_name, packages: names}
    names = payload |> :mix_hex_registry.encode_names() |> sign_and_gzip(private_key)
    write_file("#{public_dir}/names", names)

    versions = for {name, versions} <- versions, do: %{name: name, versions: versions}
    payload = %{repository: repo_name, packages: versions}
    versions = payload |> :mix_hex_registry.encode_versions() |> sign_and_gzip(private_key)
    write_file("#{public_dir}/versions", versions)
  end

  defp sign_and_gzip(protobuf, private_key) do
    protobuf
    |> :mix_hex_registry.sign_protobuf(private_key)
    |> :zlib.gzip()
  end

  defp get_private_key(path) do
    case File.read(path) do
      {:ok, data} ->
        Hex.Shell.info(["* using ", path])
        decode_private_key(data)

      {:error, :enoent} ->
        key = generate_random_private_key()
        write_file(path, encode_private_key(key))
        key
    end
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

  defp ensure_public_tarballs(public_dir) do
    path = Path.join(public_dir, "tarballs")

    unless File.dir?(path) do
      Hex.Shell.info(["* creating ", path])
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

  defp generate_random_private_key() do
    :public_key.generate_key({:rsa, 2048, 65537})
  end

  defp encode_private_key(key) do
    :public_key.pem_encode([:public_key.pem_entry_encode(:RSAPrivateKey, key)])
  end

  defp encode_public_key(key) do
    :public_key.pem_encode([:public_key.pem_entry_encode(:RSAPublicKey, key)])
  end

  defp decode_private_key(data) do
    [entry] = :public_key.pem_decode(data)
    :public_key.pem_entry_decode(entry)
  end
end
