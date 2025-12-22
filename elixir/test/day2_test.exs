defmodule Aoc25.Day2Test do
  use ExUnit.Case

  import Aoc25.Day2

  describe "valid?/1" do
    test "empty id" do
      assert valid?(nil)
    end

    test "odd length id" do
      assert valid?(123)
    end

    test "even length id" do
      assert valid?(1234)
    end

    test "id is pair" do
      refute valid?(55)
    end

    test "id matches pattern" do
      refute valid?(38_593_859)
    end
  end
end
