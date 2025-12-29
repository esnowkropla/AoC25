defmodule Aoc25.Day4Test do
  use ExUnit.Case

  import Aoc25.Day4

  @input ~s"""
  ..@@.@@@@.
  @@@.@.@.@@
  """

  @full_board ~s"""
  ..@@.@@@@.
  @@@.@.@.@@
  @@@@@.@.@@
  @.@@@@..@.
  @@.@@@@.@@
  .@@@@@@@.@
  .@.@.@.@@@
  @.@@@.@@@@
  .@@@@@@@@.
  @.@.@@@.@.
  """

  describe "#read_row" do
    setup do: %{row: "..@@.@@@@."}

    test "it reads a row", %{row: row} do
      assert MapSet.new([2, 3, 5, 6, 7, 8]) == read_row(row)
    end
  end

  describe "#read_input" do
    setup do: %{input: @input}

    test "it reads multiple rows", %{input: input} do
      expected =
        MapSet.new([
          {2, 0},
          {3, 0},
          {5, 0},
          {6, 0},
          {7, 0},
          {8, 0},
          {0, 1},
          {1, 1},
          {2, 1},
          {4, 1},
          {6, 1},
          {8, 1},
          {9, 1}
        ])

      assert expected == read_input(input)
    end
  end

  describe "#neighbours" do
    test "it generates neighbour cells" do
      cell = {4, 3}

      expected = [
        {3, 2},
        {3, 3},
        {3, 4},
        {4, 2},
        {4, 4},
        {5, 2},
        {5, 3},
        {5, 4}
      ]

      assert expected == neighbours(cell)
    end
  end

  describe "#occupied?" do
    setup do: %{board: read_input(@input)}

    test "it is true for marked positions", %{board: board} do
      assert occupied?(board, {2, 0})
    end

    test "it is false for other positions", %{board: board} do
      refute occupied?(board, {1, 0})
    end
  end

  describe "#accessible?" do
    setup do: %{board: read_input(@full_board)}

    test "it is true when < 4 neighbours occupied", %{board: board} do
      assert accessible?(board, {2, 0})
    end

    test "it is not true when many neighbours occupied", %{board: board} do
      refute accessible?(board, {4, 3})
    end
  end
end
