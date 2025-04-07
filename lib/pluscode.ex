defmodule Pluscode do
  @moduledoc """
  Convert locations to and from Open Location Codes (Plus Codes).

  Plus Codes are short, 10-11 character codes that can be used instead
  of street addresses. The codes can be generated and decoded offline, and use
  a reduced character set that minimizes the chance of codes including words.

  Codes are able to be shortened relative to a nearby location. This means that
  in many cases, only four to seven characters of the code are needed.
  To recover the original code, the same location is not required, as long as
  a nearby location is provided.

  Codes represent rectangular areas rather than points, and the longer the
  code, the smaller the area. A 10 character code represents a 13.5x13.5
  meter area (at the equator). An 11 character code represents approximately
  a 2.8x3.5 meter area.
  """
  require Logger

  # A separator used to break the code into two parts to aid memorability.
  @separator ?+

  # The number of characters to place before the separator.
  @separator_position 8

  # The character used to pad codes.
  @padding ?0
  # The character set used to encode the values.
  @code_alphabet ~c"23456789CFGHJMPQRVWX"
  @code_values @code_alphabet
               |> Enum.with_index()
               |> Map.new()

  # The base to use to convert numbers to/from.
  @encoding_base length(@code_alphabet)

  # The maximum value for latitude in degrees.
  @latitude_max 90

  # The maximum value for longitude in degrees.
  @longitude_max 180

  # The min number of digits to process in a Plus Code.
  @min_digit_count 2

  # The max number of digits to process in a Plus Code.
  @max_digit_count 15

  # Maximum code length using lat/lng pair encoding. The area of such a
  # code is approximately 13x13 meters (at the equator), and should be suitable
  # for identifying buildings. This excludes prefix and separator characters.
  @pair_code_length 10

  # First place value of the pairs (if the last pair value is 1).
  @pair_first_place_value @encoding_base |> :math.pow(@pair_code_length / 2 - 1) |> trunc()

  # Inverse of the precision of the pair section of the code.
  @pair_precision @encoding_base |> :math.pow(3) |> trunc()

  # The resolution values in degrees for each position in the lat/lng pair
  # encoding. These give the place value of each position, and therefore the
  # dimensions of the resulting area.
  @pair_resolutions [20.0, 1.0, 0.05, 0.0025, 0.000125]

  # Number of digits in the grid precision part of the code.
  @grid_code_length @max_digit_count - @pair_code_length

  # Number of columns in the grid refinement method.
  @grid_columns 4

  # Number of rows in the grid refinement method.
  @grid_rows 5

  # First place value of the latitude grid (if the last place is 1).
  @grid_lat_first_place_value @grid_rows |> :math.pow(@grid_code_length - 1) |> trunc()

  # First place value of the longitude grid (if the last place is 1).
  @grid_lng_first_place_value @grid_columns |> :math.pow(@grid_code_length - 1) |> trunc()

  # Multiply latitude by this much to make it a multiple of the finest
  # precision.
  @final_lat_precision trunc(
                         @pair_precision *
                           :math.pow(@grid_rows, @max_digit_count - @pair_code_length)
                       )

  # Multiply longitude by this much to make it a multiple of the finest
  # precision.
  @final_lng_precision trunc(
                         @pair_precision *
                           :math.pow(@grid_columns, @max_digit_count - @pair_code_length)
                       )

  # Minimum length of a code that can be shortened.
  @min_trimmable_code_len 6

  @doc """
  A struct representing the coordinates of a decoded Open Location Code.

  The coordinates include the latitude and longitude of the lower left and
  upper right corners and the center of the bounding box for the area the
  code represents.

  ## Fields

  * `:latitude_lo` - The latitude of the SW corner in degrees.
  * `:longitude_lo` - The longitude of the SW corner in degrees.
  * `:latitude_hi` - The latitude of the NE corner in degrees.
  * `:longitude_hi` - The longitude of the NE corner in degrees.
  * `:latitude_center` - The latitude of the center in degrees.
  * `:longitude_center` - The longitude of the center in degrees.
  * `:code_length` - The number of significant characters that were in the code (excluding separator).
  """
  defstruct [
    :latitude_lo,
    :longitude_lo,
    :latitude_hi,
    :longitude_hi,
    :latitude_center,
    :longitude_center,
    :code_length
  ]

  @type t :: %__MODULE__{
          latitude_lo: float(),
          longitude_lo: float(),
          latitude_hi: float(),
          longitude_hi: float(),
          latitude_center: float(),
          longitude_center: float(),
          code_length: integer()
        }

  @doc """
  Determines if a code is valid.

  To be valid, all characters must be from the Open Location Code character
  set with at most one separator. The separator can be in any even-numbered
  position up to the eighth digit.

  ## Args
    * `code` - The code to validate.

  ## Returns
    `true` if the code is valid, `false` otherwise.

  ## Examples
      iex> Pluscode.valid?("8FVC9G8F+6X")
      true

      iex> Pluscode.valid?("8FVC9G8F+")
      true

      iex> Pluscode.valid?("invalid")
      false
  """
  def valid?(code) when is_binary(code), do: valid?(String.to_charlist(code))

  def valid?(code) when is_list(code) do
    with {_, true} <- {:separator_count, count_char(code, @separator) <= 1},
         {_, true} <- {:code_length, length(code) > 1},
         sep_pos = Enum.find_index(code, &(&1 == @separator)),
         {_, true} <- {:separator_exists, not is_nil(sep_pos)},
         {_, true} <- {:separator_position, separator_valid?(sep_pos)},
         pad_pos = Enum.find_index(code, &(&1 == @padding)),
         {_, true} <- {:padding_valid, padding_valid?(pad_pos, sep_pos, code)},
         {_, true} <- {:chars_after_sep, length(code) - sep_pos - 1 != 1},
         {_, true} <- {:valid_chars, only_valid_chars?(code)} do
      true
    else
      {validation_step, false} ->
        Logger.debug("Open Location Code validation failed at: #{validation_step}")
        false
    end
  end

  defp separator_valid?(nil), do: false
  defp separator_valid?(sep_pos) when sep_pos > @separator_position, do: false
  defp separator_valid?(sep_pos) when rem(sep_pos, 2) == 1, do: false
  defp separator_valid?(_), do: true

  defp padding_valid?(nil, _sep_pos, _code), do: true
  defp padding_valid?(0, _sep_pos, _code), do: false
  defp padding_valid?(_, sep_pos, _code) when sep_pos < @separator_position, do: false

  defp padding_valid?(pad_pos, sep_pos, code) do
    # Get padding substring
    pads = Enum.slice(code, pad_pos..(sep_pos - 1))

    # Check padding conditions
    pads |> length() |> rem(2) == 0 and
      Enum.all?(pads, &(&1 == @padding)) and
      List.last(code) == @separator
  end

  defp only_valid_chars?(code) do
    valid_chars = @code_alphabet ++ [@separator, @padding]

    code
    |> upcase_list()
    |> Enum.all?(fn ch ->
      ch in valid_chars
    end)
  end

  defp count_char(list, char) do
    Enum.count(list, &(&1 == char))
  end

  @doc """
  Determines if a code is a valid short code.

  A short Open Location Code is a sequence created by removing four or more
  digits from an Open Location Code. It must include a separator character.

  ## Args
    * `code` - The code to check.

  ## Returns
    `true` if the code is a valid short code, `false` otherwise.

  ## Examples
      iex> Pluscode.short?("9G8F+6X")
      true

      iex> Pluscode.short?("8FVC9G8F+6X")
      false
  """
  def short?(code) when is_binary(code), do: short?(String.to_charlist(code))

  def short?(code) do
    if valid?(code) do
      # If there are less characters than expected before the SEPARATOR.
      case Enum.find_index(code, &(&1 == @separator)) do
        nil -> false
        0 -> true
        sep -> sep < @separator_position
      end
    else
      false
    end
  end

  @doc """
  Determines if a code is a valid full Open Location Code.

  Not all possible combinations of Open Location Code characters decode to
  valid latitude and longitude values. This checks that a code is valid
  and also that the latitude and longitude values are legal.

  ## Args
    * `code` - The code to check.

  ## Returns
    `true` if the code is a valid full code, `false` otherwise.

  ## Rules
    * If the prefix character is present, it must be the first character.
    * If the separator character is present, it must be after four characters.
    * The first latitude character must decode to a valid latitude (<90 degrees).
    * The first longitude character must decode to a valid longitude (<180 degrees).

  ## Examples
      iex> Pluscode.full?("8FVC9G8F+6X")
      true

      iex> Pluscode.full?("9G8F+6X")
      false
  """
  def full?(code) when is_binary(code), do: full?(String.to_charlist(code))

  def full?(code) do
    with {:valid, true} <- {:valid, valid?(code)},
         {:not_short, false} <- {:not_short, short?(code)},
         first_lat_value = get_alphabet_index(hd(code)) * @encoding_base,
         {:lat_range, true} <- {:lat_range, first_lat_value < @latitude_max * 2},
         {:lng_range, true} <- {:lng_range, validate_longitude_value(code)} do
      true
    else
      {:valid, false} ->
        Logger.debug("Code is not valid")
        false

      {:not_short, true} ->
        Logger.debug("Code is short")
        false

      {:lat_range, false} ->
        Logger.debug("Latitude would decode to >= 90 degrees")
        false

      {:lng_range, false} ->
        Logger.debug("Longitude would decode to >= 180 degrees")
        false
    end
  end

  defp validate_longitude_value(code) do
    first_lng_value =
      if length(code) > 1 do
        get_alphabet_index(Enum.at(code, 1)) * @encoding_base
      else
        0
      end

    first_lng_value < @longitude_max * 2
  end

  @doc false
  # Returns the index of a character in the code alphabet.
  # Handles both uppercase and lowercase characters.
  #
  # ## Args
  #   * `char` - The character to look up (as an integer).
  #
  # ## Returns
  #   The index of the character in the code alphabet, or nil if not found.
  defp get_alphabet_index(char) when is_integer(char) do
    upcase_char = if char >= ?a and char <= ?z, do: char - 32, else: char
    @code_values[upcase_char]
  end

  @doc """
  Encode a location into an Open Location Code.

  ## Args
    * `latitude` - The latitude in signed decimal degrees. Must be between -90 and 90.
    * `longitude` - The longitude in signed decimal degrees. Must be between -180 and 180.
    * `code_length` - The number of significant digits in the output code (default: 10).
                     The length determines the precision of the code.

  ## Returns
    A string containing the Open Location Code.

  ## Examples
      iex> Pluscode.encode(47.365590, 8.524997)
      "8FVC9G8F+6X"

      iex> Pluscode.encode(47.365590, 8.524997, 11)
      "8FVC9G8F+6XQ"
  """
  def encode(lat, lon, code_length \\ @pair_code_length)

  def encode(_lat, _lon, code_length) when code_length < @min_digit_count do
    raise "Invalid Open Location Code length"
  end

  def encode(_lat, _lon, code_length) when code_length < @pair_code_length and rem(code_length, 2) == 1 do
    raise ArgumentError, "Code length must be even - odd lengths have sides in ratio of 20:1"
  end

  def encode(latitude, longitude, code_length) when code_length > @max_digit_count do
    encode(latitude, longitude, @max_digit_count)
  end

  def encode(latitude, longitude, code_length) do
    latitude = clip_latitude(latitude)
    longitude = normalize_longitude(longitude)
    # Latitude 90 needs to be adjusted to be just less, so the returned code
    # can also be decoded.
    latitude =
      if latitude == 90 do
        latitude - compute_latitude_precision(code_length)
      else
        latitude
      end

    # Compute the code.
    # This approach converts each value to an integer after multiplying it by
    # the final precision. This allows us to use only integer operations, so
    # avoiding any accumulation of floating point representation errors.

    # Multiply values by their precision and convert to positive.
    # Force to integers so the division operations will have integer results.
    # Note: Python requires rounding before truncating to ensure precision!
    lat_val = trunc(Float.round((latitude + @latitude_max) * @final_lat_precision, 6))
    lng_val = trunc(Float.round((longitude + @longitude_max) * @final_lng_precision, 6))

    code = []

    {code, lat_val, lng_val} =
      if code_length > @pair_code_length do
        Enum.reduce(
          0..(@max_digit_count - @pair_code_length - 1),
          {code, lat_val, lng_val},
          fn _i, {code, lat_val, lng_val} ->
            lat_digit = rem(lat_val, @grid_rows)
            lng_digit = rem(lng_val, @grid_columns)
            ndx = lat_digit * @grid_columns + lng_digit
            new_code = [Enum.at(@code_alphabet, ndx) | code]
            new_lat_val = div(lat_val, @grid_rows)
            new_lng_val = div(lng_val, @grid_columns)
            {new_code, new_lat_val, new_lng_val}
          end
        )
      else
        lat_val = div(lat_val, trunc(:math.pow(@grid_rows, @grid_code_length)))
        lng_val = div(lng_val, trunc(:math.pow(@grid_columns, @grid_code_length)))
        {code, lat_val, lng_val}
      end

    {final_code, _, _} =
      Enum.reduce(
        0..(div(@pair_code_length, 2) - 1),
        {code, lat_val, lng_val},
        fn _i, {code, lat_val, lng_val} ->
          new_code =
            [
              Enum.at(@code_alphabet, rem(lat_val, @encoding_base)),
              Enum.at(@code_alphabet, rem(lng_val, @encoding_base)) | code
            ]

          new_lat_val = div(lat_val, @encoding_base)
          new_lng_val = div(lng_val, @encoding_base)

          {new_code, new_lat_val, new_lng_val}
        end
      )

    code =
      List.insert_at(final_code, @separator_position, @separator)

    if_result =
      if code_length >= @separator_position do
        Enum.take(code, code_length + 1)
      else
        Enum.concat([
          Enum.take(code, code_length),
          Enum.map(1..(@separator_position - code_length), fn _ -> "0" end),
          [@separator]
        ])
      end

    to_string(if_result)
  end

  @doc """
  Decodes an Open Location Code into the location coordinates.
  Returns a Pluscode struct that includes the coordinates of the bounding
  box - the lower left, center and upper right.
  Args:
    code: The Open Location Code to decode.
  Returns:
    A Pluscode struct that provides the latitude and longitude of two of the
    corners of the area, the center, and the length of the original code.
  """
  def decode(code) when is_binary(code), do: decode(String.to_charlist(code))

  def decode(code) do
    code =
      code
      |> upcase_list()
      |> Enum.reject(&(&1 in [?0, ?+]))
      |> Enum.take(@max_digit_count)

    normal_lat = -@latitude_max * @pair_precision
    normal_lng = -@longitude_max * @pair_precision
    grid_lat = 0
    grid_lng = 0
    digits = min(length(code), @pair_code_length)
    last_loop = div(digits, 2)
    pv = @pair_first_place_value

    {normal_lat, normal_lng, pv} =
      code
      |> Enum.take(digits)
      |> Enum.chunk_every(2)
      |> Enum.with_index(1)
      |> Enum.reduce(
        {normal_lat, normal_lng, pv},
        fn {[lat_char, lng_char], idx}, {lat_acc, lng_acc, pv} ->
          lat_value = get_alphabet_index(lat_char)
          lng_value = get_alphabet_index(lng_char)
          lat = lat_acc + lat_value * pv
          lng = lng_acc + lng_value * pv

          new_pv = if idx == last_loop, do: pv, else: div(pv, @encoding_base)

          {lat, lng, new_pv}
        end
      )

    lat_precision = pv / @pair_precision
    lng_precision = pv / @pair_precision

    {grid_lat, grid_lng, lat_precision, lng_precision} =
      if length(code) > @pair_code_length do
        {grid_lat, grid_lng, row_pv, col_pv} =
          code
          |> Enum.drop(@pair_code_length)
          |> Enum.reverse()
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.reduce(
            {grid_lat, grid_lng, @grid_lat_first_place_value, @grid_lng_first_place_value},
            fn {char, i}, {grid_lat, grid_lng, row_pv, col_pv} ->
              digit_val = get_alphabet_index(char)
              row = div(digit_val, @grid_columns)
              col = rem(digit_val, @grid_columns)

              new_grid_lat = grid_lat + row * row_pv
              new_grid_lng = grid_lng + col * col_pv

              if i == 0 do
                {new_grid_lat, new_grid_lng, row_pv, col_pv}
              else
                new_row_pv = div(row_pv, @grid_rows)
                new_col_pv = div(col_pv, @grid_columns)
                {new_grid_lat, new_grid_lng, new_row_pv, new_col_pv}
              end
            end
          )

        new_lat_precision = row_pv / @final_lat_precision
        new_lng_precision = col_pv / @final_lng_precision
        {grid_lat, grid_lng, new_lat_precision, new_lng_precision}
      else
        {0, 0, lat_precision, lng_precision}
      end

    lat_lo = normal_lat / @pair_precision + grid_lat / @final_lat_precision
    lng_lo = normal_lng / @pair_precision + grid_lng / @final_lng_precision

    code_length = min(length(code), @max_digit_count)

    lat_lo = Float.round(lat_lo, 14)
    lng_lo = Float.round(lng_lo, 14)
    lat_hi = Float.round(lat_lo + lat_precision, 14)
    lng_hi = Float.round(lng_lo + lng_precision, 14)

    %__MODULE__{
      latitude_lo: lat_lo,
      longitude_lo: lng_lo,
      latitude_hi: lat_hi,
      longitude_hi: lng_hi,
      latitude_center: (lat_lo + lat_hi) / 2,
      longitude_center: (lng_lo + lng_hi) / 2,
      code_length: code_length
    }
  end

  @doc """
  Remove characters from the start of an Open Location Code.

  This uses a reference location to determine how many initial characters
  can be removed from the OLC code. The number of characters that can be
  removed depends on the distance between the code center and the reference
  location.

  ## Args
    * `code` - A full Open Location Code to shorten.
    * `latitude` - The reference latitude in signed decimal degrees.
    * `longitude` - The reference longitude in signed decimal degrees.

  ## Returns
    A string containing the shortened Open Location Code.

  ## Rules
    * The minimum number of characters that will be removed is four.
    * If more than four characters can be removed, the additional characters
      will be replaced with the padding character (0).
    * At most eight characters will be removed.
    * The reference location must be within 50% of the maximum range.

  ## Examples
      iex> Pluscode.shorten("8FVC9G8F+6X", 47.5, 8.5)
      "9G8F+6X"
  """
  def shorten(code, latitude, longitude) when is_binary(code), do: shorten(String.to_charlist(code), latitude, longitude)

  def shorten(code, latitude, longitude) do
    with {_, true} <- {:full, full?(code)},
         false <- @padding in code,
         code_area = decode(code),
         true <- code_area.code_length >= @min_trimmable_code_len do
      # Ensure that latitude and longitude are valid
      latitude = clip_latitude(latitude)
      longitude = normalize_longitude(longitude)

      # How close are the latitude and longitude to the code center
      code_range =
        max(
          abs(code_area.latitude_center - latitude),
          abs(code_area.longitude_center - longitude)
        )

      # Find appropriate shortening length by checking each resolution
      # from largest to smallest
      shortened =
        @pair_resolutions
        |> Enum.with_index()
        |> Enum.reverse()
        |> Enum.drop(1)
        |> Enum.reduce_while(
          code,
          fn
            {_resolution, 0}, code ->
              {:halt, code}

            {resolution, i}, code ->
              if code_range < resolution * 0.3 do
                # We can trim it - remove 2*(i+1) characters from the start
                {:halt, Enum.drop(code, (i + 1) * 2)}
              else
                {:cont, code}
              end
          end
        )

      to_string(shortened)
    else
      {:full, false} ->
        raise "Passed code is not valid and full: #{code}"

      true ->
        raise "Cannot shorten padded codes: #{code}"

      false ->
        raise "Code length must be at least #{@min_trimmable_code_len}"
    end
  end

  @doc """
  Recover the nearest matching code to a specified location.
  Recover the nearest matching full code to a specified location.

  Given a short code of between four and seven characters, this recovers
  the nearest matching full code to the specified location.

  ## Args
    * `code` - A valid short Open Location Code.
    * `reference_latitude` - The reference latitude in signed decimal degrees.
    * `reference_longitude` - The reference longitude in signed decimal degrees.

  ## Returns
    A string containing the nearest matching full Open Location Code.

  ## Examples
      iex> Pluscode.recover_nearest("9G8F+6X", 47.4, 8.6)
      "8FVC9G8F+6X"
  """
  def recover_nearest(code, reference_latitude, reference_longitude) when is_binary(code),
    do: recover_nearest(String.to_charlist(code), reference_latitude, reference_longitude)

  def recover_nearest(code, reference_latitude, reference_longitude) do
    with_result =
      with false <- full?(code),
           true <- short?(code) do
        # Clean up and normalize inputs
        code = upcase_list(code)
        reference_latitude = clip_latitude(reference_latitude)
        reference_longitude = normalize_longitude(reference_longitude)

        # Compute padding length and resolution
        padding_length = @separator_position - Enum.find_index(code, &(&1 == @separator))
        resolution = :math.pow(20, 2 - padding_length / 2)
        half_resolution = resolution / 2.0

        # Get the reference area
        reference_prefix =
          reference_latitude
          |> encode(reference_longitude)
          |> String.slice(0, padding_length)

        code_area = decode(reference_prefix <> to_string(code))

        # Adjust latitude if needed
        code_area =
          cond do
            reference_latitude + half_resolution < code_area.latitude_center and
                code_area.latitude_center - resolution >= -@latitude_max ->
              # Move south
              %{code_area | latitude_center: code_area.latitude_center - resolution}

            reference_latitude - half_resolution > code_area.latitude_center and
                code_area.latitude_center + resolution <= @latitude_max ->
              # Move north
              %{code_area | latitude_center: code_area.latitude_center + resolution}

            true ->
              code_area
          end

        # Adjust longitude if needed
        code_area =
          cond do
            reference_longitude + half_resolution < code_area.longitude_center ->
              %{code_area | longitude_center: code_area.longitude_center - resolution}

            reference_longitude - half_resolution > code_area.longitude_center ->
              %{code_area | longitude_center: code_area.longitude_center + resolution}

            true ->
              code_area
          end

        # Encode the adjusted position
        encode(
          code_area.latitude_center,
          code_area.longitude_center,
          code_area.code_length
        )
      else
        true ->
          # If it's already a full code, just return it uppercased
          upcase_list(code)

        false ->
          raise "Passed short code is not valid - #{code}"
      end

    to_string(with_result)
  end

  @doc false
  defp clip_latitude(latitude) do
    min(90, max(-90, latitude))
  end

  @doc """
  Compute the latitude precision value for a given code length.

  ## Args
    * `code_length` - The length of the code to compute precision for.

  ## Returns
    The precision value in degrees. For lengths <= 10, latitude and longitude
    have the same precision. For lengths > 10, latitude has a different precision
    due to the grid method having fewer columns than rows.

  ## Examples
      iex> Pluscode.compute_latitude_precision(10)
      0.000125

      iex> Pluscode.compute_latitude_precision(11)
      2.5e-5
  """
  def compute_latitude_precision(code_length) do
    if code_length <= 10 do
      :math.pow(20, :math.floor(code_length / -2 + 2))
    else
      :math.pow(20, -3) / :math.pow(@grid_rows, code_length - 10)
    end
  end

  @doc """
  Normalize a longitude into the range -180 to 180, not including 180.

  ## Args
    * `longitude` - A longitude in signed decimal degrees.

  ## Returns
    The normalized longitude value.

  ## Examples
      iex> Pluscode.normalize_longitude(185)
      -175

      iex> Pluscode.normalize_longitude(-185)
      175
  """
  def normalize_longitude(longitude) when longitude < -180, do: normalize_longitude(longitude + 360)

  def normalize_longitude(longitude) when longitude >= 180, do: normalize_longitude(longitude - 360)

  def normalize_longitude(longitude), do: longitude

  @doc false
  # Converts all lowercase ASCII characters in a charlist to uppercase.
  # Non-ASCII characters and non-lowercase characters are left unchanged.
  #
  # ## Args
  #   * `code` - The charlist to convert.
  #
  # ## Returns
  #   The converted charlist with all ASCII lowercase characters converted to uppercase.
  defp upcase_list(code), do: Enum.map(code, &if(&1 in ?a..?z, do: &1 - 32, else: &1))
end
