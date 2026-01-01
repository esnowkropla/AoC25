defmodule Aoc25.Day6 do
  defmodule Operator do
    defstruct [:op, :size]
  end

  defmodule Column do
    defstruct [:values, :operator]
  end

  defmodule TransposedColumn do
    defstruct [:values, :operator]
  end

  @space 32
  @times 42
  @plus 43
  @operators [@times, @plus]

  def sum_input(input) do
    input
    |> extract_columns()
    |> Enum.map(&transpose_column/1)
    |> Enum.reduce(0, fn column, acc ->
      evaluate_column(column) + acc
    end)
  end

  def extract_columns(input) when is_binary(input) do
    extract_columns(to_charlists(input))
  end

  def extract_columns([[] | _]), do: []

  def extract_columns(input) when is_list(input) do
    op =
      input
      |> find_operator_line()
      |> parse_operator()

    {current, rest} =
      input
      |> Enum.map(&Enum.split(&1, op.size + 1))
      |> Enum.unzip()

    column_values =
      Enum.take_while(current, fn line ->
        [first | _] = line
        first not in @operators
      end)
      |> Enum.map(&Enum.take(&1, op.size))

    [%Column{values: column_values, operator: op} | extract_columns(rest)]
  end

  def to_charlists(input) when is_binary(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
    |> Enum.filter(&(Enum.count(&1) > 0))
  end

  def parse_operator(list) do
    slice =
      Enum.slice(list, 0..(next_op_idx(list) - 1))

    case slice do
      ~c"*" ++ rest ->
        %Operator{op: :mul, size: Enum.count(rest) + 1}

      ~c"+" ++ rest ->
        %Operator{op: :sum, size: Enum.count(rest) + 1}

      x ->
        raise ArgumentError, "invalid operator #{x}"
    end
  end

  def next_op_idx(charlist) do
    count = Enum.count(charlist)

    charlist
    |> Enum.slice(1..count)
    |> Enum.find_index(&(&1 != @space))
    |> then(&(&1 || count))
  end

  def find_operator_line(input) do
    Enum.find(input, fn [first | _] -> first in @operators end)
  end

  def transpose_column(%Column{values: values, operator: op}) do
    %TransposedColumn{
      values:
        values
        |> Enum.zip()
        |> Enum.map(&parse_tuple/1),
      operator: op
    }
  end

  def evaluate_column(%TransposedColumn{values: values, operator: op}) do
    case op.op do
      :mul -> eval_product(values)
      :sum -> eval_sum(values)
    end
  end

  defp parse_tuple(tuple) do
    {int, _} =
      Tuple.to_list(tuple)
      |> to_string()
      |> String.trim()
      |> Integer.parse()

    int
  end

  def build_items(input) do
    input
    |> String.split("\n")
    |> zip_lines()
    |> Enum.reduce(%{}, fn item, acc -> combine_items(item, acc) end)
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

  def robust_parse(int) when is_integer(int), do: int

  def robust_parse(str) when is_binary(str) do
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
