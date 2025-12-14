import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{unwrap}
import gleam/string
import lenient_parse
import simplifile.{read}

const dial_size = 100

pub type Rotation {
  Right(num: Int)
  Left(num: Int)
}

pub fn rotation_to_string(rotation: Rotation) -> String {
  case rotation {
    Right(num) -> "R" <> int.to_string(num)
    Left(num) -> "L" <> int.to_string(num)
  }
}

pub type Dial {
  Dial(current: Int, zeros: Int)
}

pub type RotationResult {
  RotationResult(dial: Int, zeros_hit: Int)
}

pub fn new_rotation(line: String) -> Option(Rotation) {
  case line {
    "R" <> num -> new_right(num)
    "L" <> num -> new_left(num)
    _ -> None
  }
}

fn new_right(num: String) -> Option(Rotation) {
  case lenient_parse.to_int(num) {
    Ok(num) -> Some(Right(num: num))
    Error(_err) -> None
  }
}

fn new_left(num: String) -> Option(Rotation) {
  case lenient_parse.to_int(num) {
    Ok(num) -> Some(Left(num: num))
    Error(_err) -> None
  }
}

pub fn zeros_hit(dial: Dial, rotation: Rotation) -> Int {
  case rotation {
    Right(num: num) -> { dial.current + num } / dial_size
    Left(num: num) if num >= dial.current && dial.current > 0 -> {
      { num - dial.current } / dial_size + 1
    }
    Left(num: num) if dial.current == 0 -> {
      num / dial_size
    }
    Left(_) -> 0
  }
}

pub fn rotate(dial: Dial, rotation: Rotation) -> RotationResult {
  let dial_position =
    unwrap(
      case rotation {
        Right(num: num) -> int.modulo(dial.current + num, dial_size)
        Left(num: num) -> int.modulo(dial.current - num, dial_size)
      },
      0,
    )

  RotationResult(dial_position, zeros_hit(dial, rotation))
}

pub fn update(dial: Dial, rotation: Rotation) -> Dial {
  let rotation_result = rotate(dial, rotation)

  Dial(
    current: rotation_result.dial,
    zeros: dial.zeros + rotation_result.zeros_hit,
  )
}

pub fn main() -> Nil {
  let assert Ok(contents) = read(from: "day1-input")
  let lines = string.split(contents, "\n")

  let dial: Dial =
    list.fold(
      lines,
      Dial(current: 50, zeros: 0),
      fn(dial: Dial, line: String) -> Dial {
        echo line
        case new_rotation(line) {
          Some(rotation) -> {
            update(dial, rotation)
            |> echo
          }
          None -> dial
        }
      },
    )

  echo dial

  Nil
}
