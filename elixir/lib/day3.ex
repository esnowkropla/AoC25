defmodule Aoc25.Day3 do
  def largest_pair(bank) do
    digits = get_digits(bank)
    full_count = Enum.count(digits)
    indexed_digits = Enum.zip(digits, 0..full_count)

    {first_digit, idx} =
      indexed_digits
      |> Enum.take(full_count - 1)
      |> Enum.max_by(fn {digit, _idx} ->
        digit
      end)

    second_digit =
      Enum.drop(digits, idx + 1)
      |> Enum.max()

    first_digit * 10 + second_digit
  end

  defp get_digits(bank) do
    bank
    |> Integer.parse()
    |> then(fn {num, _} -> num end)
    |> Integer.digits()
  end
end
