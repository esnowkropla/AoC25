defmodule Aoc25.Day2 do
  import Enum, only: [split: 2, count: 1]

  def valid?(id) when is_integer(id) do
    digits = Integer.digits(id)

    {first, rest} = split(digits, Integer.floor_div(count(digits), 2))

    first != rest
  end

  def valid?(_id), do: true
end
