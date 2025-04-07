defmodule CsvTest do
  @moduledoc """
  Tests for the Pluscode module using CSV test data.

  This module reads test cases from CSV files and runs them against the Pluscode implementation.
  The CSV files are located in the test_data directory at the root of the project.
  """

  use ExUnit.Case

  @encoding_csv_path "test_data/encoding.csv"
  @decoding_csv_path "test_data/decoding.csv"
  @validity_csv_path "test_data/validityTests.csv"
  @short_code_csv_path "test_data/shortCodeTests.csv"

  test "encoding from CSV" do
    @encoding_csv_path
    |> parse_csv()
    |> Enum.filter(fn row -> String.length(row) > 0 && !String.starts_with?(row, "#") end)
    |> Enum.map(&String.split(&1, ","))
    |> Enum.filter(fn parts -> length(parts) >= 4 end)
    |> Enum.each(fn parts ->
      [lat, lng, len, expected | _rest] = parts

      # Skip test cases with empty expected results (error cases)
      if String.length(expected) > 0 do
        lat = parse_float(lat)
        lng = parse_float(lng)
        len = parse_int(len)

        actual = Pluscode.encode(lat, lng, len)

        assert actual == expected,
               "Encoding #{lat}, #{lng} with length #{len}: expected #{expected}, got #{actual}"
      end
    end)
  end

  test "decoding from CSV" do
    @decoding_csv_path
    |> parse_csv()
    |> Enum.filter(fn row -> String.length(row) > 0 && !String.starts_with?(row, "#") end)
    |> Enum.map(&String.split(&1, ","))
    |> Enum.filter(fn parts -> length(parts) >= 6 end)
    |> Enum.each(fn parts ->
      # Handle the specific format in the CSV
      [code, _code_len, lat_lo, lng_lo, lat_hi, lng_hi | _rest] = parts
      lat_lo = parse_float(lat_lo)
      lng_lo = parse_float(lng_lo)
      lat_hi = parse_float(lat_hi)
      lng_hi = parse_float(lng_hi)

      area = Pluscode.decode(code)

      assert_in_delta area.latitude_lo,
                      lat_lo,
                      0.00002,
                      "Decoding #{code}: latitude_lo expected #{lat_lo}, got #{area.latitude_lo}"

      assert_in_delta area.longitude_lo,
                      lng_lo,
                      0.00002,
                      "Decoding #{code}: longitude_lo expected #{lng_lo}, got #{area.longitude_lo}"

      assert_in_delta area.latitude_hi,
                      lat_hi,
                      0.00001,
                      "Decoding #{code}: latitude_hi expected #{lat_hi}, got #{area.latitude_hi}"

      assert_in_delta area.longitude_hi,
                      lng_hi,
                      0.00001,
                      "Decoding #{code}: longitude_hi expected #{lng_hi}, got #{area.longitude_hi}"
    end)
  end

  test "validity from CSV" do
    @validity_csv_path
    |> parse_csv()
    |> Enum.filter(fn row -> String.length(row) > 0 && !String.starts_with?(row, "#") end)
    |> Enum.map(&String.split(&1, ","))
    |> Enum.filter(fn parts -> length(parts) >= 3 end)
    |> Enum.each(fn parts ->
      [code, is_valid, is_short, is_full | _rest] = parts
      is_valid = parse_bool(is_valid)
      is_short = parse_bool(is_short)
      is_full = parse_bool(is_full)

      {actual_valid, actual_short, actual_full} =
        {Pluscode.valid?(code), Pluscode.short?(code), Pluscode.full?(code)}

      assert actual_valid == is_valid,
             "Validating #{code}: valid? expected #{is_valid}, got #{actual_valid}"

      if is_valid do
        assert actual_short == is_short,
               "Validating #{code}: short? expected #{is_short}, got #{actual_short}"

        assert actual_full == is_full,
               "Validating #{code}: full expected #{is_full}, got #{actual_full}"
      end
    end)
  end

  test "short codes from CSV" do
    @short_code_csv_path
    |> parse_csv()
    |> Enum.filter(fn row -> String.length(row) > 0 && !String.starts_with?(row, "#") end)
    |> Enum.map(&String.split(&1, ","))
    |> Enum.filter(fn parts -> length(parts) >= 5 end)
    |> Enum.each(fn parts ->
      [full_code, lat, lng, short_code, test_type | _rest] = parts
      lat = parse_float(lat)
      lng = parse_float(lng)
      # Test shortening if test_type is "B" or "S"
      if test_type in ["B", "S"] do
        assert short_code == Pluscode.shorten(full_code, lat, lng),
               "Shortening #{full_code} at #{lat},#{lng}: expected #{short_code}, got #{Pluscode.shorten(full_code, lat, lng)}"
      end

      # Test recovery if test_type is "B" or "R"
      if test_type in ["B", "R"] do
        assert full_code == Pluscode.recover_nearest(short_code, lat, lng),
               "Recovering #{short_code} at #{lat},#{lng}: expected #{full_code}, got #{Pluscode.recover_nearest(short_code, lat, lng)}"
      end
    end)
  end

  # Helper functions

  defp parse_csv(path) do
    path
    |> Path.expand(__DIR__)
    |> File.read!()
    |> String.split("\n")
  end

  defp parse_float(str) do
    {float, _} = Float.parse(str)
    float
  end

  defp parse_int(str) do
    {int, _} = Integer.parse(str)
    int
  end

  defp parse_bool("true"), do: true
  defp parse_bool("1"), do: true
  defp parse_bool(_), do: false
end
