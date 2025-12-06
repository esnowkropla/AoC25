import gleam/io
import simplifile.{read}

pub fn main() -> Nil {
  let assert Ok(contents) = read(from: "day1-input")
  io.println(contents)
}
