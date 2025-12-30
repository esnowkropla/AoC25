defmodule Aoc25.Day6Test do
  use ExUnit.Case, async: true

  import Aoc25.Day6

  @input ~s"""
  123 328   51 64
   45  64  387 23
    6  98  215 314
  *   +    *   +
  """

  test "#build_items" do
    items = build_items(@input)

    total =
      items
      |> Map.values()
      |> Enum.map(&eval_item/1)
      |> Enum.sum()

    assert total == 4_277_556
  end
end
