defmodule Aoc25.Day1Test do
  use ExUnit.Case

  alias Aoc25.Day1.Dial
  alias Aoc25.Day1.Rotation
  alias Aoc25.Day1.RotationResult

  import Aoc25.Day1, only: [new_rotation: 1, rotate: 2, zeros_hit: 2, update: 2]

  describe "#new_rotation" do
    test "it parses rotations" do
      line = "R32\n"
      assert {:ok, %Rotation{direction: :right, amount: 32}} = new_rotation(line)
    end
  end

  describe "#rotate" do
    test "rotate test" do
      dial = %Dial{pos: 0, zeros: 0}
      rotation = %Rotation{direction: :right, amount: 12}
      assert %RotationResult{dial: 12} = rotate(dial, rotation)
    end

    test "rotate doesn't go negative" do
      dial = %Dial{pos: 0, zeros: 0}
      rotation = %Rotation{direction: :left, amount: 1}
      assert %RotationResult{dial: 99, zeros_hit: 0} = rotate(dial, rotation)
    end

    test "rotate wraps around the dial size" do
      dial = %Dial{pos: 99, zeros: 0}
      rotation = %Rotation{amount: 10, direction: :right}
      assert %RotationResult{dial: 9, zeros_hit: 1} = rotate(dial, rotation)
    end
  end

  describe "edge cases with rotation sequences" do
    test "left 150 from position 50" do
      dial = %Dial{pos: 50, zeros: 0}
      rotation = %Rotation{direction: :left, amount: 150}
      result = rotate(dial, rotation)

      assert result.zeros_hit == 2
    end

    test "right 50 from position 50" do
      dial = %Dial{pos: 50, zeros: 0}
      rotation = %Rotation{direction: :right, amount: 50}
      result = rotate(dial, rotation)

      assert result.zeros_hit == 1
    end

    test "left 50 then right 50 from position 50" do
      dial = %Dial{pos: 50, zeros: 0}

      rotations = [
        %Rotation{direction: :left, amount: 50},
        %Rotation{direction: :right, amount: 50}
      ]

      result = Enum.reduce(rotations, dial, &update(&2, &1))

      assert result.zeros == 1
    end

    test "left 50 then left 50 from position 50" do
      dial = %Dial{pos: 50, zeros: 0}

      rotations = [
        %Rotation{direction: :left, amount: 50},
        %Rotation{direction: :left, amount: 50}
      ]

      result = Enum.reduce(rotations, dial, &update(&2, &1))

      assert result.zeros == 1
    end

    test "left 150 then right 50 from position 50" do
      dial = %Dial{pos: 50, zeros: 0}

      rotations = [
        %Rotation{direction: :left, amount: 150},
        %Rotation{direction: :right, amount: 50}
      ]

      result = Enum.reduce(rotations, dial, &update(&2, &1))

      assert result.zeros == 2
    end
  end

  describe "#zeros_hit" do
    test "no zeros hit" do
      dial = %Dial{pos: 0, zeros: 0}
      rotation = %Rotation{direction: :right, amount: 10}

      assert 0 = zeros_hit(dial, rotation)
    end

    test "one zero hit" do
      dial = %Dial{pos: 95, zeros: 0}
      rotation = %Rotation{direction: :right, amount: 10}

      assert 1 = zeros_hit(dial, rotation)
    end

    test "multiple zeros hit" do
      dial = %Dial{pos: 95, zeros: 0}
      rotation = %Rotation{amount: 210, direction: :right}

      assert 3 = zeros_hit(dial, rotation)
    end

    test "one exact hit" do
      dial = %Dial{pos: 99, zeros: 0}
      rotation = %Rotation{direction: :right, amount: 1}

      assert 1 = zeros_hit(dial, rotation)
    end

    test "multiple exact hits" do
      dial = %Dial{pos: 0, zeros: 0}
      rotation = %Rotation{direction: :right, amount: 200}

      assert 2 = zeros_hit(dial, rotation)
    end

    test "no hits left" do
      dial = %Dial{pos: 10, zeros: 0}
      rotation = %Rotation{direction: :left, amount: 5}

      assert 0 = zeros_hit(dial, rotation)
    end

    test "one hit left" do
      dial = %Dial{pos: 20, zeros: 0}
      rotation = %Rotation{direction: :left, amount: 21}

      assert 1 = zeros_hit(dial, rotation)
    end

    test "multiple hits left" do
      dial = %Dial{pos: 50, zeros: 0}
      rotation = %Rotation{direction: :left, amount: 251}

      assert 3 = zeros_hit(dial, rotation)
    end

    test "one exact hit left" do
      dial = %Dial{pos: 10, zeros: 0}
      rotation = %Rotation{direction: :left, amount: 10}

      assert 1 = zeros_hit(dial, rotation)
    end

    test "multiple exact hits left" do
      dial = %Dial{pos: 21, zeros: 0}
      rotation = %Rotation{direction: :left, amount: 221}

      assert 3 = zeros_hit(dial, rotation)
    end

    test "massive rotation" do
      dial = %Dial{pos: 50, zeros: 0}
      rotation = %Rotation{direction: :right, amount: 1000}

      assert 10 = zeros_hit(dial, rotation)
    end
  end
end
