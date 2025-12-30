defmodule Aoc25.Day5 do
  alias Aoc25.Stack

  @range_pattern ~r/(\d+)-(\d+)/
  @id_pattern ~r/^(\d+)$/m

  def total_range_size(fresh_ranges) do
    fresh_ranges
    |> Enum.sort()
    |> then(&compact_ranges/1)
    |> Enum.map(&Range.size(&1))
    |> Enum.sum()
  end

  def compact_ranges([first | rest]) do
    Enum.reduce(
      rest,
      [first],
      fn range, ranges ->
        {highest, stack} = Stack.pop(ranges)

        case compact_pair(highest, range) do
          [compacted] ->
            Stack.push(stack, compacted)

          [left, right] ->
            stack
            |> Stack.push(left)
            |> Stack.push(right)
        end
      end
    )
    |> Enum.reverse()
  end

  def compact_pair(l0..l1//_ = left, r0..r1//_ = right) do
    if Range.disjoint?(l0..l1, r0..r1) do
      [left, right]
    else
      [Enum.min([l0, r0])..Enum.max([l1, r1])]
    end
  end

  def set_from_range(range, set) do
    Enum.reduce(range, set, fn id, ids -> MapSet.put(ids, id) end)
  end

  def fresh_ids(fresh_ranges, ids) do
    Enum.filter(ids, fn id ->
      Enum.any?(fresh_ranges, fn range -> id in range end)
    end)
  end

  def parse_ranges(input) do
    Regex.scan(@range_pattern, input, capture: :all_but_first)
    |> Enum.map(&make_range/1)
  end

  def parse_ids(input) do
    Regex.scan(@id_pattern, input, capture: :all_but_first)
    |> Enum.map(fn [id_str] ->
      {id, _} = Integer.parse(id_str)

      id
    end)
  end

  defp make_range([start_str, finish_str]) do
    {start, _} = Integer.parse(start_str)
    {finish, _} = Integer.parse(finish_str)

    start..finish
  end
end
