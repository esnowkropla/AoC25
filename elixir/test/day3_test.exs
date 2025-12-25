defmodule Aoc25.Day3Test do
  use ExUnit.Case, async: true

  import Aoc25.Day3

  describe "#largest_subsequence" do
    test "first example" do
      bank = "987654321111111"

      assert largest_subsequence(bank, 12) == 987_654_321_111
    end

    test "second example" do
      bank = "811111111111119"

      assert largest_subsequence(bank, 12) == 811_111_111_119
    end

    test "third example" do
      bank = "234234234234278"

      assert largest_subsequence(bank, 12) == 434_234_234_278
    end

    test "fourth example" do
      bank = "818181911112111"

      assert largest_subsequence(bank, 12) == 888_911_112_111
    end
  end
end
