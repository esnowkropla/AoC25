defmodule Aoc25.Day3 do
  def largest_subsequence(bank, sequence_size) do
    digits = get_digits(bank)
    find_digits(digits, 1, sequence_size, [])
  end

  def get_digits(bank) do
    bank
    |> Integer.parse()
    |> then(fn {num, _} -> num end)
    |> Integer.digits()
  end

  def combine(digits) do
    digits
    |> Enum.zip(0..Enum.count(digits))
    |> Enum.map(fn {digit, power} ->
      digit * 10 ** power
    end)
    |> Enum.sum()
  end

  def find_digits(digits, n, n, acc) do
    digits
    |> Enum.max()
    |> then(fn last_digit -> [last_digit | acc] end)
    |> combine()
  end

  def find_digits(digits, n, sequence_size, acc) do
    count = Enum.count(digits)
    indexed_digits = Enum.zip(digits, 0..count)

    {next_digit, idx} =
      indexed_digits
      |> Enum.take(count - sequence_size + n)
      |> Enum.max_by(fn {digit, _idx} -> digit end)

    remaining_digits = Enum.drop(digits, idx + 1)
    find_digits(remaining_digits, n + 1, sequence_size, [next_digit | acc])
  end
end
