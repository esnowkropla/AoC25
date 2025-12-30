defmodule Aoc25.Day6 do
  def build_items(input) do
    input
    |> String.split("\n")
    |> zip_lines()
    |> Enum.reduce(%{}, fn item, acc -> Aoc25.Day6.combine_items(item, acc) end)
  end

  def eval_item([op | terms]) do
    case op do
      "*" -> eval_product(terms)
      "+" -> eval_sum(terms)
      _ -> raise ArgumentError, "Unknown operator"
    end
  end

  def eval_product(terms) do
    terms
    |> Enum.map(&robust_parse/1)
    |> Enum.reduce(fn x, acc -> acc * x end)
  end

  def eval_sum(terms) do
    terms
    |> Enum.map(&robust_parse/1)
    |> Enum.sum()
  end

  def robust_parse(str) do
    {int, _} = Integer.parse(str)

    int
  end

  def zip_lines(lines) do
    Enum.map(lines, fn line ->
      Stream.from_index() |> Stream.zip(String.split(line)) |> Enum.into(%{})
    end)
  end

  def combine_items(row, map) do
    Enum.reduce(row, map, fn {k, v}, acc ->
      {_, map} =
        Map.get_and_update(acc, k, fn current ->
          case current do
            nil -> {current, [v]}
            rest -> {current, [v | rest]}
          end
        end)

      map
    end)
  end
end
