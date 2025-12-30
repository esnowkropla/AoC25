defmodule Aoc25.Day5Test do
  use ExUnit.Case, async: true

  import Aoc25.Day5

  @full_input ~s"""
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32
  """

  describe "#parse_ranges" do
    test "it parses fresh id ranges" do
      assert parse_ranges(@full_input) == [
               3..5,
               10..14,
               16..20,
               12..18
             ]
    end
  end

  describe "#parse_ids" do
    test "it parses ids" do
      assert parse_ids(@full_input) == [1, 5, 8, 11, 17, 32]
    end
  end

  describe "#fresh_ids" do
    test "it checks ids against fresh ranges" do
      fresh_ranges = parse_ranges(@full_input)
      available_ids = parse_ids(@full_input)
      assert [5, 11, 17] == fresh_ids(fresh_ranges, available_ids)
    end
  end

  describe "#total_range_size" do
    test "it sums the total size of the fresh ranges" do
      fresh_ranges = parse_ranges(@full_input)

      assert 14 == total_range_size(fresh_ranges)
    end
  end

  describe "#compact_pair" do
    test "it compacts simple overlaps" do
      left = 1..3
      right = 2..6
      assert [1..6] == compact_pair(left, right)
    end

    test "it compacts complete overlaps" do
      left = 1..6
      right = 2..4
      assert [1..6] == compact_pair(left, right)
    end

    test "it returns disjoint ranges" do
      left = 1..3
      right = 6..10
      assert [1..3, 6..10] == compact_pair(left, right)
    end
  end

  describe "#compact_ranges" do
    test "it compacts ranges" do
      ranges =
        @full_input
        |> parse_ranges()
        |> Enum.sort()

      assert compact_ranges(ranges) == [
               3..5,
               10..20
             ]
    end
  end
end
