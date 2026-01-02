defmodule Aoc25.Day7 do
  defmodule Cell do
    defstruct [:val, :x, :y]
  end

  defmodule Grid do
    defstruct [:map, :count]

    def get_cell(%__MODULE__{} = grid, coords) do
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
      grid_line
      |> make_cells(y)
      |> Enum.reduce(grid_struct, &reduce_line/2)
    end)
  end

  def make_cells(grid_line, y) do
    grid_line
    |> Stream.zip(Stream.from_index())
    |> Enum.map(fn {grid_character, x} ->
      %Cell{val: grid_character, x: x, y: y}
    end)
  end

  def reduce_line(%Cell{} = cell, %Grid{} = grid) do
    above_cell = Grid.get_cell(grid, {cell.x, cell.y - 1})

    case {above_cell, cell.val} do
      {:empty, _} ->
        grid

      {:source, ?.} ->
        put_beam(grid, {cell.x, cell.y})

      {:source, ?^} ->
        grid
        |> put_beam({cell.x - 1, cell.y})
        |> put_beam({cell.x + 1, cell.y})
        |> inc_count()

      {:beam, ?.} ->
        put_beam(grid, {cell.x, cell.y})

      {:beam, ?^} ->
        grid
        |> put_beam({cell.x - 1, cell.y})
        |> put_beam({cell.x + 1, cell.y})
        |> inc_count()

      _ ->
        grid
    end
  end

  def put_beam(%Grid{map: map} = grid, {x, y}) do
    new_map = Map.put(map, {x, y}, :beam)

    %Grid{grid | map: new_map}
  end

  def inc_count(%Grid{} = grid), do: put_in(grid.count, grid.count + 1)

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
