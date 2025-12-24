defmodule Aoc25.Day3Test do
  use ExUnit.Case, async: true

  import Aoc25.Day3

  describe "#largest_pair" do
    test "first example" do
      bank = "987654321111111"

      assert largest_pair(bank) == 98
    end

    test "second example" do
      bank = "811111111111119"

      assert largest_pair(bank) == 89
    end

    test "third example" do
      bank = "234234234234278"

      assert largest_pair(bank) == 78
    end

    test "fourth example" do
      bank = "818181911112111"

      assert largest_pair(bank) == 92
    end
  end
end
