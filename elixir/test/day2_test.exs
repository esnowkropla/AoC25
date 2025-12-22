defmodule Aoc25.Day2Test do
  use ExUnit.Case

  import Aoc25.Day2

  describe "invalid?/1" do
    test "empty id" do
      refute invalid?(nil)
    end

    test "single digit id" do
      refute invalid?(2)
    end

    test "odd length id" do
      refute invalid?(123)
    end

    test "even length id" do
      refute invalid?(1234)
    end

    test "id is pair" do
      assert invalid?(55)
    end

    test "id matches pattern" do
      assert invalid?(38_593_859)
    end

    test "short pattern repeated multiples" do
      assert invalid?(111)
    end

    test "long pattern repeated multiples" do
      assert invalid?(121_212)
    end
  end
end
