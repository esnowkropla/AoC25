defmodule Aoc25.Day2 do
  import Enum, only: [split: 2, count: 1]

  @pattern ~r/(\d+)-(\d+)/

  def pattern, do: @pattern

  def invalid?(id) when is_integer(id) do
    digits = Integer.digits(id)

    {first, rest} = split(digits, Integer.floor_div(count(digits), 2))

    first == rest
  end

  def invalid?(_id), do: false

  def process_input(text, workers \\ 1) do
    Regex.scan(@pattern, text, capture: :all_but_first)
    |> Enum.map(&make_range/1)
    |> Task.async_stream(&find_invalid/1, max_concurrency: workers)
    |> Task.async_stream(&sum_ids/1)
    |> Enum.reduce(0, fn {:ok, sum}, acc ->
      sum + acc
    end)
  end

  defp make_range([start_str, finish_str]) do
    {start, _} = Integer.parse(start_str)
    {finish, _} = Integer.parse(finish_str)

    start..finish
  end

  defp find_invalid(range), do: Enum.filter(range, &invalid?/1)

  defp sum_ids({:ok, ids}) do
    Enum.sum(ids)
  end
end
