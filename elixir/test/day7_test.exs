defmodule Aoc25.Day7Test do
  use ExUnit.Case, async: true

  import Aoc25.Day7

  @input ~s"""
  .......S.......
  ...............
  .......^.......
  ...............
  ......^.^......
  ...............
  .....^.^.^.....
  ...............
  ....^.^...^....
  ...............
  ...^.^...^.^...
  ...............
  ..^...^.....^..
  ...............
  .^.^.^.^.^...^.
  ...............
  """

  describe "#iterate_grid" do
    setup [:raw_grid]

    test "it counts the splitter hits", %{raw_grid: grid} do
      out_grid = iterate_grid(grid)
      assert out_grid.count == 21
    end
  end

  describe "#find_source" do
    setup [:raw_grid]

    test "it finds the source location", %{raw_grid: grid} do
      assert {:ok, {7, 0}} == find_source(grid)
    end
  end

  describe "#parse_input" do
    setup [:raw_grid]

    test "it produces charlists", %{raw_grid: grid} do
      assert is_list(hd(grid))
    end

    test "it filters empty lines", %{raw_grid: grid} do
      assert Enum.all?(grid, &(Enum.count(&1) > 0))
    end
  end

  defp raw_grid(_), do: %{raw_grid: parse_input(@input)}
end
