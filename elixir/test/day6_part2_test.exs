defmodule Aoc25.Day6Part2Test do
  use ExUnit.Case, async: true

  alias Aoc25.Day6.Column
  alias Aoc25.Day6.Operator
  alias Aoc25.Day6.TransposedColumn

  import Aoc25.Day6

  @input ~s"""
  123 328  51 64 
   45 64  387 23 
    6 98  215 314
  *   +   *   +  
  """

  @lists [
    ~c"123 328  51 64 ",
    ~c" 45 64  387 23 ",
    ~c"  6 98  215 314",
    ~c"*   +   *   +  "
  ]

  test "to_charlists converts the string" do
    assert to_charlists(@input) == @lists
  end

  test "find operator line" do
    assert ~c"*   +   *   +  " == find_operator_line(@lists)
  end

  describe "sum_input" do
    test "it sums the example input" do
      assert 3_263_827 == sum_input(@input)
    end
  end

  describe "#parse_operator/1" do
    test "parses multiplication" do
      op_str = find_operator_line(@lists)

      assert %Operator{op: :mul, size: 3} = parse_operator(op_str)
    end

    test "parses the last column" do
      op_str = ~c"+  "

      assert %Operator{op: :sum, size: 3} = parse_operator(op_str)
    end
  end

  describe "#next_op_idx" do
    test "parses the first operator" do
      op_str = find_operator_line(@lists)

      assert 3 == next_op_idx(op_str)
    end

    test "it parses the last operator" do
      op_str = ~c"+  "

      assert 3 == next_op_idx(op_str)
    end
  end

  describe "#extract_columns" do
    test "it extracts the first column" do
      values = [~c"123", ~c" 45", ~c"  6"]
      operator = %Operator{op: :mul, size: 3}

      assert [%Column{values: ^values, operator: ^operator} | _] = extract_columns(@input)
    end
  end

  describe "transpose column" do
    test "it assembles the column values" do
      values = [~c"123", ~c" 45", ~c"  6"]
      col = %Column{values: values}

      assert %TransposedColumn{values: [1, 24, 356]} = transpose_column(col)
    end
  end

  describe "evaluate column" do
    test "it sums the column" do
      values = [369, 248, 8]
      col = %TransposedColumn{values: values, operator: %Operator{op: :sum}}
      assert evaluate_column(col) == 625
    end

    test "it multiplies the column" do
      values = [32, 581, 175]
      col = %TransposedColumn{values: values, operator: %Operator{op: :mul}}
      assert evaluate_column(col) == 3_253_600
    end
  end
end
