defmodule Aoc25.Day1 do
  @dial_size 100
  defmodule Dial do
    defstruct [:pos, :zeros]
  end

  defmodule Rotation do
    defstruct [:direction, :amount]
  end

  defmodule RotationResult do
    defstruct [:dial, :zeros_hit]
  end

  def new_rotation("R" <> amount) do
    case Integer.parse(amount) do
      {num, _} -> {:ok, %Rotation{direction: :right, amount: num}}
      :error -> :error
    end
  end

  def new_rotation("L" <> amount) do
    case Integer.parse(amount) do
      {num, _} -> {:ok, %Rotation{direction: :left, amount: num}}
      :error -> :error
    end
  end

  def new_rotation(_), do: :error

  def zeros_hit(%Dial{} = dial, %Rotation{direction: :right, amount: amount}) do
    Integer.floor_div(dial.pos + amount, @dial_size)
  end

  def zeros_hit(%Dial{pos: pos}, %Rotation{direction: :left, amount: amount})
      when amount >= pos and pos > 0 do
    Integer.floor_div(amount - pos, @dial_size) + 1
  end

  def zeros_hit(%Dial{pos: pos}, %Rotation{direction: :left, amount: amount}) when pos == 0 do
    Integer.floor_div(amount, @dial_size)
  end

  def zeros_hit(_, _), do: 0

  def rotate(%Dial{} = dial, r = %Rotation{direction: :right, amount: amount}) do
    %RotationResult{
      dial: Integer.mod(dial.pos + amount, @dial_size),
      zeros_hit: zeros_hit(dial, r)
    }
  end

  def rotate(%Dial{} = dial, r = %Rotation{direction: :left, amount: amount}) do
    %RotationResult{
      dial: Integer.mod(dial.pos - amount, @dial_size),
      zeros_hit: zeros_hit(dial, r)
    }
  end

  def update(%Dial{} = dial, %Rotation{} = rotation) do
    result = rotate(dial, rotation)
    %Dial{pos: result.dial, zeros: dial.zeros + result.zeros_hit}
  end
end
