import aoc25/day1.{type Rotation, Dial, Left, Right, RotationResult}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string

pub fn new_rotation_test() {
  let line = "R32\n"
  let assert Some(Right(num: 32)) = day1.new_rotation(line)
}

pub fn rotate_test() {
  let dial = Dial(current: 0, zeros: 0)
  let rotation = Right(num: 12)

  let assert RotationResult(dial: 12, zeros_hit: 0) =
    day1.rotate(dial, rotation)
}

pub fn rotate_cant_go_negative_test() {
  let dial = Dial(current: 0, zeros: 0)
  let rotation = Left(num: 1)

  let assert RotationResult(dial: 99, zeros_hit: 0) =
    day1.rotate(dial, rotation)
}

pub fn rotate_wraps_around_one_hundred_test() {
  let dial = Dial(current: 99, zeros: 0)
  let rotation = Right(num: 10)

  let assert RotationResult(dial: 9, zeros_hit: 1) = day1.rotate(dial, rotation)
}

pub fn no_zeros_hit_test() {
  let dial = Dial(current: 0, zeros: 0)
  let rotation = Right(num: 10)

  let assert 0 = day1.zeros_hit(dial, rotation)
}

pub fn one_zero_hit_test() {
  let dial = Dial(current: 95, zeros: 0)
  let rotation = Right(num: 10)

  let assert 1 = day1.zeros_hit(dial, rotation)
}

pub fn multiple_zeros_hit_test() {
  let dial = Dial(current: 95, zeros: 0)
  let rotation = Right(num: 210)

  let assert 3 = day1.zeros_hit(dial, rotation)
}

pub fn one_exact_hit_test() {
  let dial = Dial(current: 99, zeros: 0)
  let rotation = Right(num: 1)

  let assert 1 = day1.zeros_hit(dial, rotation)
}

pub fn multiple_exact_hits_test() {
  let dial = Dial(current: 0, zeros: 0)
  let rotation = Right(200)

  let assert 2 = day1.zeros_hit(dial, rotation)
}

pub fn no_hits_left_test() {
  let dial = Dial(current: 10, zeros: 0)
  let rotation = Left(num: 5)

  let assert 0 = day1.zeros_hit(dial, rotation)
}

pub fn one_hit_left_test() {
  let dial = Dial(current: 20, zeros: 0)
  let rotation = Left(num: 21)

  let assert 1 = day1.zeros_hit(dial, rotation)
}

pub fn multiple_hits_left_test() {
  let dial = Dial(current: 50, zeros: 0)
  let rotation = Left(num: 251)

  let assert 3 = day1.zeros_hit(dial, rotation)
}

pub fn one_exact_hit_left_test() {
  let dial = Dial(current: 10, zeros: 0)
  let rotation = Left(num: 10)

  let assert 1 = day1.zeros_hit(dial, rotation)
}

pub fn multiple_exact_hits_left_test() {
  let dial = Dial(current: 21, zeros: 0)
  let rotation = Left(221)

  let assert 3 = day1.zeros_hit(dial, rotation)
}

pub fn massive_rotation_test() {
  let dial = Dial(current: 50, zeros: 0)
  let rotation = Right(1000)

  let assert 10 = day1.zeros_hit(dial, rotation)
}

pub fn edge_case_test() {
  let test_cases = [
    #(50, [Left(150)], 2),
    #(50, [Right(50)], 1),
    #(50, [Left(50), Right(50)], 1),
    #(50, [Left(50), Left(50)], 1),
    #(50, [Left(150), Right(50)], 2),
  ]

  list.map(test_cases, fn(x: #(Int, List(Rotation), Int)) -> Nil {
    let #(current, moves, expected) = x
    let dial = Dial(current:, zeros: 0)
    let result: day1.Dial =
      list.fold(moves, dial, fn(dial, rotation: Rotation) -> day1.Dial {
        day1.update(dial, rotation)
      })

    let msg =
      "current: "
      <> int.to_string(current)
      <> " moves: "
      <> string.join(list.map(moves, day1.rotation_to_string), ", ")
      <> " expected: "
      <> int.to_string(expected)

    assert result.zeros == expected as msg
  })
}
