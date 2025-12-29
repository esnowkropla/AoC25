defmodule Aoc25.Day4 do
  @full ?@

  def occupied?(board, coords) do
    MapSet.member?(board, coords)
  end

  def accessible?(board, cell) do
    neighbours(cell)
    |> Enum.map(fn cell -> if occupied?(board, cell), do: 1 end)
    |> Enum.filter(&(&1 != nil))
    |> Enum.sum()
    |> then(fn sum -> sum < 4 end)
  end

  def neighbours({x0, y0}) do
    for x <- [x0 - 1, x0, x0 + 1], y <- [y0 - 1, y0, y0 + 1], x != x0 or y != y0 do
      {x, y}
    end
  end

  def read_row(row) do
    row
    |> String.to_charlist()
    |> Stream.zip(Stream.from_index())
    |> Stream.filter(fn {char, _idx} -> char == @full end)
    |> Stream.map(fn {_char, idx} -> idx end)
    |> Enum.into(%MapSet{})
  end

  def map_row(row, n) do
    row
    |> read_row()
    |> MapSet.new(fn x -> {x, n} end)
  end

  def read_input(rows) do
    rows
    |> String.split()
    |> Stream.zip(Stream.from_index())
    |> Stream.flat_map(fn {row, idx} -> map_row(row, idx) end)
    |> MapSet.new()
  end
end
