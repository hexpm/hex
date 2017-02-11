defmodule Mix.Tasks.Hex.Repo do
  use Mix.Task

  @shortdoc "Manages Hex repositories"

  @moduledoc """
  Manages the list of available Hex repositories.

  To use a package from another repository add `repo: :my_repo` to the
  dependency declaration in `mix.exs`:

      {:plug, "~> 1.0", repo: :my_repo}

  By default all dependencies of plug will also be fetched from `:my_repo`
  unless plug has declared otherwise in its dependency definition.

  To use packages from `:my_repo` you need to add it to your configuration
  first. You do that by calling `mix hex.repo my_repo https://myrepo.example.com`.

  The default repo is called `:hexpm` and points to https://repo.hex.pm. This
  can be overridden by using `mix hex.repo set ...`.

  Child dependencies will always be fetched from the same repository as the
  parent package. To override which repository a package is fetched from add
  the package to your dependencies and add the `:repo` option.

  ### Add a repo

      mix hex.repo add NAME URL [PUBLIC_KEY_PATH [AUTH_KEY]]

  ### Set config for repo

      mix hex.repo set url NAME URL
      mix hex.repo set public_key NAME PUBLIC_KEY_PATH
      mix hex.repo set auth_key NAME AUTH_KEY

  ### Remove repo

      mix hex.repo remove NAME

  ### List all repos

      mix hex.repo list
  """

  def run(args) do
    Hex.start
    {_, args, _} = OptionParser.parse(args)

    case args do
      ["add", name, url | rest] when length(rest) <= 2 ->
        add(name, url, rest)
      ["set", "url", name, url] ->
        set_url(name, url)
      ["set", "public_key", name, public_key] ->
        set_public_key(name, public_key)
      ["set", "auth_key", name, auth_key] ->
        set_auth_key(name, auth_key)
      ["remove", name] ->
        remove(name)
      ["list"] ->
        list()
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.repo add NAME URL [PUBLIC_KEY_PATH [AUTH_KEY]]
        mix hex.repo set url NAME URL
        mix hex.repo set public_key NAME PUBLIC_KEY_PATH
        mix hex.repo set auth_key NAME AUTH_KEY
        mix hex.repo remove NAME
        mix hex.repo show NAME
        mix hex.repo list
        """
    end
  end

  # TODO: read and verify public key

  defp add(name, url, rest) do
    {public_key, auth_key} = extra_add_args(rest)
    public_key = read_public_key(public_key)
    repo = %{url: url, public_key: public_key, auth_key: auth_key}

    read_config()
    |> Map.put(name, repo)
    |> Hex.Config.update_repos
  end

  defp set_url(name, url) do
    read_config()
    |> Map.update!(name, &Map.put(&1, :url, url))
    |> Hex.Config.update_repos
  end

  defp set_public_key(name, public_key) do
    public_key = read_public_key(public_key)
    read_config()
    |> Map.update!(name, &Map.put(&1, :public_key, public_key))
    |> Hex.Config.update_repos
  end

  defp set_auth_key(name, auth_key) do
    read_config()
    |> Map.update!(name, &Map.put(&1, :auth_key, auth_key))
    |> Hex.Config.update_repos
  end

  defp remove(name) do
    read_config()
    |> Map.delete(name)
    |> Hex.Config.update_repos
  end

  defp list do
    header = ["Name", "URL", "Public key", "Auth key"]
    values =
      Enum.map(read_config(), fn {name, %{url: url, public_key: public_key, auth_key: auth_key}} ->
        [name, url, show_public_key(public_key), auth_key]
      end)
    Mix.Tasks.Hex.print_table(header, values)
  end

  defp read_public_key(nil), do: nil
  defp read_public_key(path) do
    key =
      path
      |> Path.expand
      |> File.read!

    decode_public_key(key)
    key
  end

  defp decode_public_key(key) do
    [pem_entry] = :public_key.pem_decode(key)
    :public_key.pem_entry_decode(pem_entry)
  rescue
    _ ->
      Mix.raise """
      Could not decode public key. The public key contents are shown below.

      #{key}

      Public keys must be valid and be in the PEM format.
      """
  end

  defp read_config do
    Hex.Config.read
    |> Hex.Config.read_repos
  end

  defp show_public_key(nil), do: nil
  defp show_public_key(public_key) do
    [pem_entry] = :public_key.pem_decode(public_key)
    public_key = :public_key.pem_entry_decode(pem_entry)
    ssh_hostkey_fingerprint(public_key)
  end

  defp extra_add_args([]), do: {nil, nil}
  defp extra_add_args([public_key]), do: {public_key, nil}
  defp extra_add_args([public_key, auth_key]), do: {public_key, auth_key}

  # Adapted from https://github.com/erlang/otp/blob/3eddb0f762de248d3230b38bc9d478bfbc8e7331/lib/public_key/src/public_key.erl#L824
  defp ssh_hostkey_fingerprint(key) do
    "SHA256:#{sshfp_string(key)}"
  end

  defp sshfp_string(key) do
    :crypto.hash(:sha256, :public_key.ssh_encode(key, :ssh2_pubkey))
    |> Base.encode64(padding: false)
  end
end
