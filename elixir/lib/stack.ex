defmodule Aoc25.Stack do
  def new(), do: []

  def pop([]), do: nil

  def pop([elem | rest]), do: {elem, rest}

  def push(stack, item), do: [item | stack]
end
