defmodule Aoc25.Day7 do
  defmodule Cell do
    defstruct [:val, :x, :y]
  end

  defmodule Grid do
    defstruct [:map, :count]

    def get_cell(%__MODULE__{} = grid, {} = coords) do
      Map.get(
        grid.map,
        coords,
        :empty
      )
    end
  end

  def iterate_grid(grid) do
    {:ok, source_coords} = find_source(grid, 0)

    grid_struct = %Grid{map: %{source_coords => :source}, count: 0}
    y_size = Enum.count(grid)

    grid
    |> Enum.zip(0..y_size)
    |> Enum.slice(1..y_size)
    |> Enum.reduce(grid_struct, fn {grid_line, y}, grid_struct ->
      line =
        grid_line
        |> Stream.zip(Stream.from_index())
        |> Enum.map(fn {grid_character, x} ->
          %Cell{val: grid_character, x: x, y: y}
        end)

      Enum.reduce(line, grid_struct, &reduce_line/2)
    end)
  end

  def reduce_line(%Cell{} = cell, %Grid{} = grid) do
    case cell.val do
      ?^ -> put_in
    end
  end

  def parse_input(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
    |> Enum.filter(&(Enum.count(&1) > 0))
  end

  def find_source([first | _], n \\ 0) when is_list(first) do
    case Enum.find_index(first, &(&1 == ?S)) do
      nil -> :error
      x -> {:ok, {x, n}}
    end
  end
end
