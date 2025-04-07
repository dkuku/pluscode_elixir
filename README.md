# Pluscode

An Elixir implementation of Open Location Code (Plus Codes), a technology developed by Google that encodes location into a short, easy to share code. For more information, visit [plus.codes](https://plus.codes/).

## Features

- Encode latitude/longitude to Plus Codes
- Decode Plus Codes to coordinates
- Validate Plus Codes
- Shorten codes relative to a reference location
- Recover full codes from shortened versions
- Full compatibility with the official specification

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `pluscode` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:pluscode, "~> 0.1.0"}
  ]
end
```

## Usage

```elixir
# Encode a location
iex> Pluscode.encode(47.365590, 8.524997)
"8FVC9G8F+6X"

# Encode with higher precision
iex> Pluscode.encode(47.365590, 8.524997, 11)
"8FVC9G8F+6XQ"

# Decode a code
iex> area = Pluscode.decode("8FVC9G8F+6X")
iex> {area.latitude_center, area.longitude_center}
{47.365625, 8.525}

# Validate a code
iex> Pluscode.valid?("8FVC9G8F+6X")
true

# Shorten a code (using a reference location)
iex> Pluscode.shorten("8FVC9G8F+6X", 47.5, 8.5)
"9G8F+6X"

# Recover a full code from a short code
iex> Pluscode.recover_nearest("9G8F+6X", 47.4, 8.6)
"8FVC9G8F+6X"
```

## Documentation

Detailed documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/pluscode>.

## License

This implementation is licensed under the Apache License, Version 2.0. See the LICENSE file for details.
