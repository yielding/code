use std::float;
use std::num::atan;

fn angle(vector: (float, float)) -> float {
  let pi = float::consts::pi;
  match vector {
    (0f32, y) if y < 0f32 => 1.5 * pi,
    (0f32, y)             => 0.5 * pi,
    (x, y)                => atan(y/x)
  }
}

fn main() {

}
