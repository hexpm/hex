defmodule Hex.OptionParser do
  if Version.compare(System.version, "1.3.0") == :lt do
    def parse!(argv, opts \\ []) when is_list(argv) and is_list(opts) do
      case OptionParser.parse(argv, opts) do
        {parsed, args, []} -> {parsed, args}
        {_, _, errors} -> raise format_errors(errors, opts)
      end
    end

    defp to_underscore(option),
      do: to_underscore(option, <<>>)
    defp to_underscore("_" <> _rest, _acc),
      do: nil
    defp to_underscore("-" <> rest, acc),
      do: to_underscore(rest, acc <> "_")
    defp to_underscore(<<c>> <> rest, acc),
      do: to_underscore(rest, <<acc::binary, c>>)
    defp to_underscore(<<>>, acc),
      do: acc

    defp get_option_key(option, allow_nonexistent_atoms?) do
      if string = to_underscore(option) do
        to_existing_key(string, allow_nonexistent_atoms?)
      end
    end

    defp to_existing_key(option, true),
      do: String.to_atom(option)
    defp to_existing_key(option, false) do
      try do
        String.to_existing_atom(option)
      rescue
        ArgumentError -> nil
      end
    end

    defp format_errors([_ | _] = errors, opts) do
      types = opts[:switches] || opts[:strict]
      error_count = length(errors)
      error = if error_count == 1, do: "error", else: "errors"
      "#{error_count} #{error} found!\n" <>
        Enum.map_join(errors, "\n", &format_error(&1, opts, types))
    end

    defp format_error({option, nil}, opts, types) do
      if type = get_type(option, opts, types) do
        "#{option} : Missing argument of type #{type}"
      else
        "#{option} : Unknown option"
      end
    end

    defp format_error({option, value}, opts, types) do
      type = get_type(option, opts, types)
      "#{option} : Expected type #{type}, got #{inspect value}"
    end

    defp get_type(option, opts, types) do
      allow_nonexistent_atoms? = opts[:allow_nonexistent_atoms] || false
      key = option |> String.lstrip(?-) |> get_option_key(allow_nonexistent_atoms?)

      if option_key = opts[:aliases][key] do
        types[option_key]
      else
        types[key]
      end
    end
  else
    defdelegate parse!(argv, opts \\ []), to: OptionParser
  end
end
